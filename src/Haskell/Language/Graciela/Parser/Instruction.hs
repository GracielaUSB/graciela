{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}

module Language.Graciela.Parser.Instruction
  ( instruction
  , declarationBlock
  , declarationOrRead
  , block
  , assign
  , assign'
  , random
  , guard
  , write
  , new
  , free
  , abort
  , conditional
  , reading
  , repetition
  , skip
  ) where
-------------------------------------------------------------------------------
import           Language.Graciela.AST.Declaration    (Declaration)
import           Language.Graciela.AST.Definition     (Definition (..),
                                                       Definition' (..))
import           Language.Graciela.AST.Expression     (Expression (..), Value(..),
                                                       Expression' (..))
import qualified Language.Graciela.AST.Expression     as E (loc)
import           Language.Graciela.AST.Instruction    (Guard, Instruction (..),
                                                       Instruction' (..))
import           Language.Graciela.AST.Object
import qualified Language.Graciela.AST.Object         as O (inner, loc)
import           Language.Graciela.AST.Struct         (Struct (..))
import           Language.Graciela.AST.Type           (ArgMode (..), Type (..),
                                                       fillType, hasDT,
                                                       hasTypeVar, notConst, (=:=))
import           Language.Graciela.Common
import           Language.Graciela.Entry
import           Language.Graciela.Error
import           Language.Graciela.Location
import           Language.Graciela.Parser.Assertion   hiding (bound)
import qualified Language.Graciela.Parser.Assertion   as A (bound)
import           Language.Graciela.Parser.Declaration
import           Language.Graciela.Parser.Expression
import           Language.Graciela.Parser.Monad
import           Language.Graciela.Parser.State
import           Language.Graciela.Parser.Type
import           Language.Graciela.SymbolTable
import           Language.Graciela.Token
import           Language.Graciela.Treelike
-------------------------------------------------------------------------------
import           Control.Lens                         (use, (%=), (+=), (.=), (&),
                                                       (^.), _Just, element, (.~))
import           Control.Monad                        (foldM, unless, void,
                                                       when, zipWithM)
import           Control.Monad.Identity               (Identity)
import qualified Data.Array                           as Array (listArray)
import           Data.Foldable                        (asum, foldMap)
import qualified Data.List                            as L (find)
import qualified Data.Map.Strict                      as Map (lookup, toList)
import           Data.Monoid                          (First (..))
import           Data.Sequence                        (Seq, (<|), (|>))
import qualified Data.Sequence                        as Seq (empty, fromList,
                                                              singleton, zip)
import qualified Data.Set                             as Set
import           Data.Text                            (Text, pack, takeWhile,
                                                       unpack)
import           Prelude                              hiding (lookup, takeWhile)
import           Text.Megaparsec                      (between, eitherP,
                                                       getPosition, lookAhead,
                                                       notFollowedBy, optional,
                                                       try, (<|>))
-------------------------------------------------------------------------------

instruction :: Parser (Maybe Instruction)
instruction
   =  conditional
  <|> try procedureCall
  <|> try assign
  <|> abort
  <|> warn
  <|> repetition
  <|> skip
  <|> block


assertionInst :: Parser (Maybe Instruction)
assertionInst = do
  from <- getPosition
  expr <- assertion
  to   <- getPosition
  pure $ case expr of
    Nothing -> Nothing
    Just e -> Just Instruction
      { instLoc = Location (from,to)
      , inst'   = Assertion e
      }


declarationBlock :: Parser (Maybe (Seq Declaration))
declarationBlock = sequence <$> (declaration `endBy` match' TokSemicolon)


declarationOrRead :: Parser (Maybe (Seq (Either Declaration Instruction)))
declarationOrRead = sequence <$> (p `endBy` match' TokSemicolon)
  where
    p = do
      line <- eitherP declaration reading
      pure $ case line of
        Left  (Just l) -> Just (Left l)
        Right (Just l) -> Just (Right l)
        _              -> Nothing

block :: Parser (Maybe Instruction)
block = do
  from <- getPosition
  match TokOpenBlock
  symbolTable %= openScope from

  decls       <- declarationBlock
  actions     <- many (assertedInst $ match TokCloseBlock)
  match' TokCloseBlock
  to <- getPosition

  symbolTable %= closeScope to

  let loc = Location (from, to)

  if null actions
    then do
      putError from EmptyBlock
      pure Nothing
  else pure $ case (asum <$> sequence actions, decls) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just blockInsts, Just blockDecs) -> Just Instruction
      { instLoc = loc
      , inst'   = Block
        { blockDecs
        , blockInsts }}


assertedInst :: Parser follow -> Parser (Maybe (Seq Instruction))
assertedInst follow = do
  pos <- getPosition
  a1   <- many assertionInst
  inst <- (if null a1 then (Just <$>) else optional) instruction
  a2   <- many assertionInst
  void (lookAhead follow) <|> void (match' TokSemicolon)
  pure . sequence $ a1 <> (Seq.fromList . toList $ inst) <> a2

assign :: Parser (Maybe Instruction)
assign = assign' False

assign' :: Bool -> Parser (Maybe Instruction)
assign' cr = do
  from <- getPosition
  lvals <- expression `sepBy1` match TokComma
  match' TokAssign
  exprs <- if cr 
    then do 

      doingCoupleRel .= True
      s <- expression `sepBy` match TokComma
      doingCoupleRel .= False
      pure s
    else 
      expression `sepBy` match TokComma


  to <- getPosition
  if length lvals == length exprs
    then do
      assignPairs' <- foldM checkType (Just Seq.empty) (Seq.zip lvals exprs)
      pure $ case assignPairs' of
        Nothing -> Nothing
        Just assignPairs -> Just Instruction
          { instLoc = Location (from, to)
          , inst'   = Assign assignPairs }
    else do
      putError from . UnknownError $
        "The number of lvals does not match the number of rvals."
      pure Nothing

  where
    {- Checks if the left expression is a valid lval and
       if the assigned expression has the correct type
    -}
    checkType :: Maybe (Seq (Object, Expression))
              -> (Maybe Expression, Maybe Expression)
              -> Parser (Maybe (Seq (Object, Expression)))
    checkType _   (Nothing, _) = pure Nothing
    checkType _   (_, Nothing) = pure Nothing
    checkType acc (Just lval, Just rval) = case (lval,rval) of
      ( Expression { loc = Location (from1,_), expType = t1, expConst, exp' = Obj o},
        Expression { expType = t2, exp' = expr } )
        | not expConst && notConst o ->
          if t1 =:= t2 && assignable t1
            then case expr of
              NullPtr -> pure $ (|> (o, rval {expType = t1})) <$> acc
              _       -> pure $ (|> (o, rval)) <$> acc
            else do
              putError from1 . UnknownError $
                "Can't assign an expression of type " <>
                show t2 <> " to a variable of type " <>
                show t1 <> "."
              pure Nothing
        | otherwise -> do
          putError from1 . UnknownError $
            "The constant `" <> show o <> "` cannot be the target of an \
            \assignment."
          pure Nothing
      (Expression { loc = Location (from,_) }, Expression {}) -> do
        putError from $ UnknownError
          "An expression cannot be the target of an assignment."
        pure Nothing

    assignable a = case a of
      (GTuple _ _) -> False
      (GArray _ _) -> False
      t            | t =:= GADataType -> False
      _            -> True

random :: Parser (Maybe Instruction)
random = do
  from  <- getPosition

  id    <- identifier
  mexpr <- parens expression

  to    <- getPosition
  let loc = Location (from,to)
  case mexpr of
    Nothing -> pure Nothing
    Just expr -> case expr of
      Expression { expType, exp' = Obj o }
        | expType =:= randomizable && notConst o ->
          pure . Just $ Instruction loc (Random o)
        | expType =:= randomizable && not (notConst o) -> do
          putError from . UnknownError $
            "Cannot assign random value to a `Const` mode variable."
          pure Nothing
        | not (expType =:= randomizable) -> do
          putError from . UnknownError $
            "Cannot assign random value to a variable of type `" <>
            show expType <> "`."
          pure Nothing
      _ -> do
        putError from . UnknownError $
          "Cannot assign random value to an expression."
        pure Nothing
  where
    randomizable = GOneOf [GInt, GFloat, GBool, GChar]


-- | Parse `write` or `writeln` instructions.
write :: Parser (Maybe Instruction)
write = do

  from <- getPosition
  id <- identifier
  let ln = id == "writeln"
  exprs <- parens $ expression `sepBy` match TokComma

  to <- getPosition
  let loc = Location (from,to)
  mexprs <- foldM write' (Just Seq.empty) exprs


  pure $ case mexprs of
    Nothing  | null exprs -> Just Instruction
      { instLoc = loc
      , inst'   = Write
        { ln -- if `ln` then `writeln`, else `write`
        , wexprs = Seq.empty }}

    Just wexprs -> Just Instruction
      { instLoc = loc
      , inst'   = Write
        { ln -- if `ln` then `writeln`, else `write`
        , wexprs }}

    _ -> Nothing

  where
    write' _   Nothing = pure Nothing
    write' acc (Just e@Expression { E.loc = Location (from, _), expType })
      | expType =:= writable && expType /= GPointer GChar = do
          pure $ (|> e) <$> acc
      | otherwise = do
        p <- use pragmas
        let ok = Set.member MemoryOperations p
        if ok && expType =:= GPointer GAny
          then pure $ (|> e) <$> acc
        else do
          putError from . UnknownError $
            "Cannot write expression of type " <> show expType <> "."
          pure Nothing

    writable = GOneOf [GBool, GChar, GInt, GFloat, GString, GATypeVar ]


-- | Parse the `read` instruction.
reading :: Parser (Maybe Instruction)
reading = do

  from <- getPosition

  identifier
  ids <- parens $ expression `sepBy` match TokComma
  file <- optional fileFrom

  to <- getPosition
  let loc = Location (from, to)

  mids <- foldM read' (Just Seq.empty) ids

  case mids of
    Nothing -> pure Nothing
    Just vars -> if null vars
      then do
        putError from . UnknownError $
          "At least one object must be read in a read instruction."
        pure $ Just Instruction
          { instLoc = loc
          , inst'   = Read
            { file
            , vars }}
      else do
        case file of
          Nothing       -> pure ()
          Just fileName -> filesToRead %= Set.insert (unpack fileName)
        pure $ Just Instruction
          { instLoc = loc
          , inst'   = Read
            { file
            , vars }}
  where
    fileFrom = do
      loc <- match TokFrom
      str <- lookAhead stringLit <|>
             (putError (pos loc) (UnknownError "The name of a file must be a string") >> lookAhead stringLit)
      expression
      pure str

    read' _   Nothing = pure Nothing
    read' acc (Just Expression { exp' = Obj o @ Object { O.loc = Location (from, _), objType } })
      | objType =:= readable && notConst o =
        pure $ (|> o) <$> acc
      | notConst o = do
        putError from . UnknownError $
          "Cannot read object of type " <> show objType <> "."
        pure Nothing
      | objType =:= readable = do
        putError from . UnknownError $
          "Cannot read a `Const` mode object."
        pure Nothing
      | otherwise = do
        putError from . UnknownError $
          "Cannot read a `Const` mode object of type " <> show objType <> "."
        pure Nothing
    read' acc (Just expr@Expression { E.loc = Location (from, _) }) = do
      putError from . UnknownError $
        "Cannot read expression `" <> show expr <> "`."
      pure Nothing

    readable = GOneOf [GInt, GFloat, GChar, GBool, GATypeVar]



newOrFree :: (Object -> Type -> Instruction')
          -> String
          -> Parser (Maybe Instruction)
newOrFree inst name = do
  
  from <- getPosition
  id <- identifier
  obj <- parens expression
  to <- getPosition
  let loc = Location (from, to)

  case obj of
    Nothing -> pure Nothing
    Just e -> case e of
      Expression { expType = GPointer t, exp' = Obj o } ->
        pure . Just $ Instruction loc (inst o t)
      _ -> do
        putError from . UnknownError $ name <> " can only recive pointers."
        pure Nothing


new, free :: Parser (Maybe Instruction)
new  = newOrFree New  "New"
free = newOrFree Free "Free"


abort :: Parser (Maybe Instruction)
abort = do
  lookAhead $ match TokAbort

  from <- getPosition
  match TokAbort
  to <- getPosition
  pure . Just $ Instruction (Location (from, to)) Abort


warn :: Parser (Maybe Instruction)
warn = do
  lookAhead $ match TokWarn

  from <- getPosition
  match TokWarn
  to <- getPosition
  pure . Just $ Instruction (Location (from, to)) Warn


-- | Parse guards for both repetition and conditional
guard :: Parser (Maybe Guard)
guard = do
  from <- getPosition
  ow' <- head <$> use otherwises
  let ow = case ow' of
          Nothing -> False
          Just x  -> x
  cond <- expression
  case cond of
    Just e ->
      if isTrue e && ow then  
        otherwises %= (\x -> x & element 0 .~ (Just True))
      else if ow then 
        putError from . UnknownError $ 
          "This guard will never be reached.\n\t"
          <> "A previous guard always evaluates true"
      else pure ()
    _ -> pure ()
  match TokArrow

  symbolTable %= openScope from
  decls   <- declarationBlock
  actions <- many . assertedInst $ oneOf [TokFi, TokOd, TokSepGuards]

  to <- getPosition
  let loc = Location (from, to)

  symbolTable %= closeScope to

  if null actions
    then do
      putError from EmptyBlock
      pure Nothing
    else pure $ (\x y z -> (x,y,z)) <$> cond <*> decls <*> (asum <$> sequence actions)

-- | Auxiliar function used in `conditional` and `guard`
isTrue :: Expression -> Bool
isTrue Expression{expConst, exp' = Value (BoolV True)} = expConst
isTrue _ = False

conditional ::  Parser (Maybe Instruction)
conditional = do
  lookAhead $ match TokIf

  from <- getPosition
  match TokIf
  otherwises %= (:) (Just False)
  gs <- guard `sepBy` match TokSepGuards
  otherwises %= tail


  match' TokFi
  to <- getPosition
  let loc = Location (from, to)

  if null gs
    then do
      putError from $ UnknownError "Conditional expression without guards."
      pure Nothing
    else pure $ case sequence gs of
      Nothing  -> Nothing
      Just gs' -> do 
        let gs'' = Seq.fromList . reverse $ f (toList gs') [] Nothing
        Just $ Instruction (Location (from, to)) (Conditional gs'')
  where 
    gIsTrue :: Guard -> Bool
    gIsTrue (e,_,_) = isTrue e
    
    f :: [Guard] -> [Guard] -> Maybe Guard -> [Guard]
    f [] xs' (Just x) = x:xs' 
    f [] xs' Nothing  = xs' 
    f (x:xs) xs' g = if gIsTrue x 
      then f xs xs' (Just x)
      else f xs (x:xs') g

-- | Parse the instruction `do .. od`.
repetition :: Parser (Maybe Instruction)
repetition = do
  lookAhead $ oneOf [TokLeftInv, TokLeftBound, TokDo]

  from <- getPosition

  inv <- assertion' invariant NoDoInvariant
  bnd <- assertion' A.bound   NoDoBound

  match TokDo
  otherwises %= (:) Nothing
  gs <- guard `sepBy` match TokSepGuards
  otherwises %= tail
  match' TokOd
  to <- getPosition

  let loc = Location (from, to)

  if null gs
    then do
      putError from $ UnknownError "Repeat expression without guards."
      pure Nothing
    else pure $ repeat' loc <$> sequence gs <*> inv <*> bnd

  where
    assertion' p e = p <|> recover
      where
        recover = do
          pos <- getPosition
          putError pos e
          pure Nothing

    repeat' instLoc rguards rinv rbound = Instruction
      { instLoc
      , inst' = Repeat
        { rguards
        , rinv
        , rbound }}


-- | Parse procedure calls.
procedureCall :: Parser (Maybe Instruction)
procedureCall = do
  procName <- lookAhead identifier
  if procName == "new"    then new
  else if procName == "free"    then free
  else if procName == "write"   then write
  else if procName == "writeln" then write
  else if procName == "read"    then reading
  else procedureCall'

procedureCall' :: Parser (Maybe Instruction)
procedureCall' = do

  procName <- identifier
  from <- getPosition
  args <- parens $ expression `sepBy` match TokComma

  to <- getPosition
  let loc = Location (from, to)

  defs <- use definitions
  let nArgs   = length args

  case procName `Map.lookup` defs of
    Just Definition { defLoc, def' = GracielaProc { pSignatures }} -> do
      let args' = sequence args

      case args' of 
        Just args'' -> case pSignatures (fmap expType args'') of 
          Right (procName', argModes) -> pure $ Just Instruction
            { instLoc = loc
            , inst' = ProcedureCall
              { pName = procName'
              , pArgs = Seq.zip args'' argModes
              , pRecursiveCall = False
              , pRecursiveProc = False
              , pStructArgs    = Nothing }}
          Left message -> do
            putError from message
            pure Nothing
        _ -> pure Nothing

    Just Definition { defLoc, def' = ProcedureDef { procParams, procRecursive }} -> do
      let
        nParams = length procParams
        Location (pos, _) = defLoc
      if nArgs == nParams
        then do
          args' <- foldM (checkType procName pos) (Just Seq.empty) (Seq.zip args procParams)
          pure $ case args' of
            Nothing -> Nothing
            Just args'' -> Just Instruction
              { instLoc = loc
              , inst' = ProcedureCall
                { pName = procName
                , pArgs = args''
                , pRecursiveCall = False
                , pRecursiveProc = procRecursive
                , pStructArgs    = Nothing }}
        else do
          putError from BadProcNumberOfArgs
            { pName = procName
            , pPos  = pos
            , nParams
            , nArgs }
          pure Nothing

    Just Definition { defLoc, def' = FunctionDef {} } -> do
      putError from . UnknownError $
        "Cannot call function `" <> unpack procName <> "`, defined at " <>
        show defLoc <> "` as an instruction; a procedure was expected."
      pure Nothing

    Nothing -> do
      -- Function `p` will be used only if the procedure we are looking for is not
      -- the current procedure and is not defined yet. 
      -- In that case, it should be a Data Type procedure (or an error, of course).
      -- There are two cases: 1) It's being called outside a Data Type.
      --                      2) It's being called in another procedure inside the same Data Type
      --                      
      --                      Solution: Look for first arguments with type `GDataType`
      let
        f = case hasDTType args of
          Nothing -> do
            let
              args' = sequence args
            case args' of
              Nothing -> do
                putError from . UnknownError $ "Calling procedure `" <>
                  unpack procName <>"` with bad arguments"
                pure Nothing
              Just args'' -> do
                putError from $ UndefinedProcedure procName args''
                pure Nothing

          Just t@(GDataType name _ t') -> use currentStruct >>= \case
            Nothing -> do
              getStruct name >>= \case
                Nothing -> do
                  putError from $ UnknownError $ "Couldn't find data type " <> show t
                  pure Nothing
                Just Struct { structProcs } -> do
                  case procName `Map.lookup` structProcs of
                    Just Definition { def' = ProcedureDef { procParams, procRecursive }} -> do
                      cs <- use currentStruct
                      let
                        nParams = length procParams
                        typeArgs = case cs of
                            Nothing -> t'
                            Just (GDataType _ _ dtArgs, _, _, _) ->
                              fmap (fillType dtArgs) t'

                      when (nArgs /= nParams) . putError from . UnknownError $
                        "Calling procedure `" <> unpack procName <> "` with a bad number of arguments."

                      args' <- foldM (checkType' typeArgs procName from)
                        (Just Seq.empty) (Seq.zip args procParams)
                      pure $ case args' of
                        Nothing -> Nothing
                        Just args'' -> Just Instruction
                          { instLoc = loc
                          , inst' = ProcedureCall
                            { pName = procName
                            , pArgs = args''
                            , pRecursiveCall = False
                            , pRecursiveProc = procRecursive
                            , pStructArgs    = Just (name, typeArgs) } }

                    _ -> do
                      procs <- use definitions
                      putError from . UnknownError $
                        "Data Type `" <> unpack name <>
                        "` does not have a procedure called `" <>
                        unpack procName <> "`"
                      return Nothing

            Just (dt@GDataType {typeArgs}, _, _, _) | not (t =:= dt) ->
              getStruct name >>= \case
              Nothing -> internal $ "Could not find DT " <> show name
              Just Struct{structProcs} ->
                case procName `Map.lookup` structProcs of
                  Just Definition { def' = ProcedureDef { procParams, procRecursive }} -> do
                    let
                      nParams = length procParams
                      ta' = fmap (fillType typeArgs) t'

                    when (nArgs /= nParams) . putError from . UnknownError $
                      "Calling procedure `" <> unpack procName <>
                      "` with a bad number of arguments."

                    args' <- foldM (checkType' ta' procName from)
                      (Just Seq.empty) (Seq.zip args procParams)
                    pure $ case args' of
                      Nothing -> Nothing
                      Just args'' -> Just Instruction
                        { instLoc = loc
                        , inst' = ProcedureCall
                          { pName = procName
                          , pArgs = args''
                          , pRecursiveCall = False
                          , pRecursiveProc = procRecursive
                          , pStructArgs    = Just (name, ta') } }

                  Nothing -> do
                    procs <- use definitions
                    putError from . UnknownError $
                      "Data Type `" <> unpack name <>
                      "` does not have a procedure called `" <>
                      unpack procName <> "`"
                    return Nothing

            Just (dt@GDataType {typeArgs}, _, sp, _) -> case procName `Map.lookup` sp of
              Just Definition { def' = ProcedureDef { procParams, procRecursive }} -> do
                let
                  nParams = length procParams

                when (nArgs /= nParams) . putError from . UnknownError $
                  "Calling procedure `" <> unpack procName <> "` with a bad number of arguments."

                args' <- foldM (checkType' typeArgs procName from)
                  (Just Seq.empty) (Seq.zip args procParams)
                pure $ case args' of
                  Nothing -> Nothing
                  Just args'' -> Just Instruction
                    { instLoc = loc
                    , inst' = ProcedureCall
                      { pName = procName
                      , pArgs = args''
                      , pRecursiveCall = False
                      , pRecursiveProc = procRecursive
                      , pStructArgs    = Just (name, typeArgs) } }

              Just Definition { def' = FunctionDef {}, defLoc } -> do
                putError from . UnknownError $
                  "Cannot call function `" <> unpack procName <> "`, defined at " <>
                  show defLoc <> "` as an instruction; a procedure was expected."
                pure Nothing

              Nothing -> do
                procs <- use definitions
                putError from . UnknownError $
                  "Data Type `" <> unpack name <>
                  "` does not have a procedure called `" <>
                  unpack procName <> "`"
                return Nothing


      -- If the procedure is not defined, it's possible that we're
      -- dealing with a recursive call. The information of a procedure
      -- that is being defined is stored temporarily at the
      -- Parser.State `currentProc`.
      currentProcedure <- use currentProc
      case currentProcedure of
        Just cr@CurrentRoutine { _crTypeArgs}
          | cr^.crName == procName && cr^.crRecAllowed -> do
            let
              nParams = length (cr^.crParams)
            if nArgs == nParams
              then do
                args' <- foldM (checkType procName (cr^.crPos)) (Just Seq.empty) (Seq.zip args (cr^.crParams))

                currentProc . _Just . crRecursive .= True

                pure $ case args' of
                  Nothing -> Nothing
                  Just pArgs -> Just Instruction
                    { instLoc = loc
                    , inst' = ProcedureCall
                      { pName = procName
                      , pArgs
                      , pRecursiveCall = True
                      , pRecursiveProc = True
                      , pStructArgs    = _crTypeArgs }}
              else do
                putError from BadProcNumberOfArgs
                  { pName = procName
                  , pPos  = cr ^. crPos
                  , nParams
                  , nArgs }
                pure Nothing
          | cr^.crName == procName && not (cr^.crRecAllowed) -> do
            putError from . UnknownError $
              "Procedure `" <> unpack procName <> "` cannot call itself \
              \recursively because no bound was given for it."
            pure Nothing

          | otherwise -> f

        {- The procedure is neither a global procedure nor the current one,
           but it might be a struct's procedure so we check for that. -}
        Nothing -> f


  where
    hasDTType = getFirst . foldMap aux
    aux (Just Expression { expType = f } ) = First $ hasDT f
    aux Nothing                            = First Nothing

    checkType = checkType' (Array.listArray (0,-1) [])

    checkType' _ _ _ _ (Nothing, _) = pure Nothing
    checkType' typeArgs pName pPos acc
      (Just e@Expression { E.loc = Location (from, _), expType, exp'}, (name, pType, mode)) =
        let
          fType = fillType typeArgs pType
        in if expType =:= fType && (hasTypeVar fType || not (hasTypeVar expType))
          then case exp' of
            Obj {} | mode `elem` [Out, InOut, Ref] -> do
              pure $ (|> (e{expType = expType <> fType}, mode)) <$> acc

            _ | mode == Const -> --if expConst e
              -- then pure $ (|> (e{expType = expType <> fType}, mode)) <$> acc
              -- else do
              --   putError from . UnknownError $
              --     "The parameter `" <> unpack name <> "` has mode " <>
              --     show mode <> "\n\tbut the given expression is not constant \n"
              --   pure Nothing
                  pure $ (|> (e{expType = expType <> fType}, mode)) <$> acc
            _ | mode == In -> do
              pure $ (|> (e{expType = expType <> fType}, mode)) <$> acc

            _ -> do
              putError from . UnknownError $
                "The parameter `" <> unpack name <> "` has mode " <>
                show mode <> "\n\tbut recived an expression instead \
                \of a variable"
              pure Nothing
          else do
            putError from $
              BadProcedureArgumentType name pName pPos fType expType
            pure Nothing


-- | Parse `skip` instruction.
skip :: Parser (Maybe Instruction)
skip = do
  from <- getPosition
  match TokSkip
  to <- getPosition
  pure . Just $ Instruction (Location (from, to)) Skip
