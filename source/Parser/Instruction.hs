{-# LANGUAGE NamedFieldPuns #-}
module Parser.Instruction
  ( instruction
  , declarationBlock
  , declarationOrRead
  , block
  , assign
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
import           AST.Declaration        (Declaration)
import           AST.Definition         (Definition (..), Definition' (..))
import           AST.Expression         (Expression (..), Expression' (..),
                                         Object (..))
import qualified AST.Expression         as E (loc)
import           AST.Instruction        (Guard, Instruction (..),
                                         Instruction' (..))
import           AST.Object
import qualified AST.Object             as O (loc)
import           AST.Struct             (Struct (..))
import           Entry
import           Error
import           Location
import           Parser.Assertion       hiding (bound)
import qualified Parser.Assertion       as A (bound)
import           Parser.Declaration
import           Parser.Expression
import           Parser.Monad
-- import           Parser.Rhecovery
import           Parser.State
import           Parser.Type
import           SymbolTable
import           Token
import           Treelike
import           Type                   (ArgMode (..), Type (..), llvmName,
                                         (=:=))
-------------------------------------------------------------------------------
import           Control.Lens           (use, (%=), (+=), (^.))
import           Control.Monad          (foldM, unless, void, when, zipWithM)
import           Control.Monad.Identity (Identity)
import           Data.Foldable          (asum, toList)
import           Data.Functor           (($>))
import qualified Data.List              as L (any, find)
import qualified Data.Map               as Map (lookup)
import           Data.Monoid            ((<>))
import           Data.Sequence          (Seq, (<|), (|>))
import qualified Data.Sequence          as Seq (empty, fromList, singleton, zip)
import qualified Data.Set               as Set
import           Data.Text              (Text, pack, takeWhile, unpack)
import           Debug.Trace
import           Prelude                hiding (lookup, takeWhile)
import           Text.Megaparsec        (between, eitherP, getPosition,
                                         lookAhead, notFollowedBy, optional,
                                         try, (<|>))
-------------------------------------------------------------------------------
import           System.IO.Unsafe


instruction :: Parser (Maybe Instruction)
instruction
   =  try procedureCall
  <|> try assign
  <|> abort
  <|> warn
  <|> conditional
  <|> free
  <|> new
  <|> random
  <|> reading
  <|> repetition
  <|> skip
  <|> write -- includes write and writeln
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
declarationBlock = sequence <$> (declaration `endBy` match TokSemicolon)


declarationOrRead :: Parser (Maybe (Seq (Either Declaration Instruction)))
declarationOrRead = sequence <$> (p `endBy` match TokSemicolon)
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
  a1   <- many assertionInst
  inst <- (if null a1 then (Just <$>) else optional) instruction
  a2   <- many assertionInst
  void (lookAhead follow) <|> void (match' TokSemicolon)
  pure . sequence $ a1 <> (Seq.fromList . toList $ inst) <> a2


assign :: Parser (Maybe Instruction)
assign = do
  from <- getPosition

  lvals <- expression `sepBy1` match TokComma
  match' TokAssign
  exprs <- expression `sepBy1` match TokComma

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
    checkType acc (Just l, Just r) = case (l,r) of
      (Expression (Location (from1,_)) t1 (Obj o), Expression _ t2 _)
        | notIn o ->
          if (t1 =:= t2) && (t1 =:= GOneOf [GInt, GFloat, GBool, GChar, GPointer GAny])
            then pure $ (|> (o, r)) <$> acc
            else do
              putError from1 . UnknownError $
                "Can't assign an expression of type `" <>
                show t2 <> "` to a variable of type `" <>
                show t1 <> "`."
              pure Nothing
        | otherwise -> do
          putError from1 . UnknownError $
            "The variable `" <> show o <> "` cannot be the target of an \
            \assignment because it has mode `In`."
          pure Nothing
      (Expression (Location (from,_)) _ _, Expression {}) -> do
        putError from $ UnknownError
          "An expression cannot be the target of an assignment."
        pure Nothing


random :: Parser (Maybe Instruction)
random = do
  from  <- getPosition

  match TokRandom
  mexpr <- parens expression

  to    <- getPosition
  let loc = Location (from,to)
  case mexpr of
    Nothing -> pure Nothing
    Just expr -> case expr of
      Expression { expType, exp' = Obj o }
        | expType =:= randomizable && notIn o ->
          pure . Just $ Instruction loc (Random o)
        | expType =:= randomizable && not (notIn o) -> do
          putError from . UnknownError $
            "Cannot assign random value to an `In` mode variable."
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
  lookAhead $ oneOf [TokWrite, TokWriteln]

  from <- getPosition

  ln <- match TokWrite $> False <|> match TokWriteln $> True
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
      | expType =:= writable =
        pure $ (|> e) <$> acc
      | otherwise = do
        putError from . UnknownError $
          "Cannot write expression of type `" <> show expType <> "`."
        pure Nothing

    writable = GOneOf [GBool, GChar, GInt, GFloat, GString]
      -- TODO How will this interact with polymorphism?


-- | Parse the `read` instruction.
reading :: Parser (Maybe Instruction)
reading = do
  lookAhead $ match TokRead

  from <- getPosition

  match TokRead
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
        pure Nothing
      else do
        case file of
          Nothing -> pure ()
          Just fileName -> filesToRead %= Set.insert (unpack fileName)
        pure $ Just Instruction
          { instLoc = loc
          , inst'   = Read
            { file
            , vars }}
  where
    fileFrom = match TokFrom *> stringLit

    read' _   Nothing = pure Nothing
    read' acc (Just Expression { exp' = Obj o @ Object { O.loc = Location (from, _), objType } })
      | objType =:= readable && notIn o =
        pure $ (|> o) <$> acc
      | notIn o = do
        putError from . UnknownError $
          "Cannot read object of type `" <> show objType <> "`."
        pure Nothing
      | objType =:= readable = do
        putError from . UnknownError $
          "Cannot read an `In` mode object."
        pure Nothing
      | otherwise = do
        putError from . UnknownError $
          "Cannot read an `In` mode object of type `" <> show objType <> "`."
        pure Nothing
    read' acc (Just expr@Expression { E.loc = Location (from, _) }) = do
      putError from . UnknownError $
        "Cannot read expression `" <> show expr <> "`."
      pure Nothing

    readable = GOneOf [GInt, GFloat, GChar]
      -- TODO Maybe Booleans too?


newOrFree :: Token
          -> (Object -> Type -> Instruction')
          -> String
          -> Parser (Maybe Instruction)
newOrFree tok inst name = do
  lookAhead $ match tok

  from <- getPosition

  match tok
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
new  = newOrFree TokNew  New  "New"
free = newOrFree TokFree Free "Free"


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

  cond <- expression
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


conditional ::  Parser (Maybe Instruction)
conditional = do
  lookAhead $ match TokIf

  from <- getPosition
  match TokIf

  gs <- guard `sepBy` match TokSepGuards

  match' TokFi
  to <- getPosition
  let loc = Location (from, to)

  if null gs
    then do
      putError from $ UnknownError "Conditional expression without guards."
      pure Nothing
    else pure $ case sequence gs of
      Nothing  -> Nothing
      Just gs' -> Just $ Instruction (Location (from, to)) (Conditional gs')


-- | Parse the instruction `do .. od`.
repetition :: Parser (Maybe Instruction)
repetition = do
  lookAhead $ oneOf [TokLeftInv, TokLeftBound, TokDo]

  from <- getPosition

  inv <- assertion' invariant NoDoInvariant
  bnd <- assertion' A.bound   NoDoBound

  match TokDo
  gs <- guard `sepBy` match TokSepGuards
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
  from <- getPosition

  procName <- identifier
  args <- between (match TokLeftPar) (match' TokRightPar) $
    expression `sepBy` match TokComma

  to <- getPosition
  let loc = Location (from, to)

  defs <- use definitions

  case procName `Map.lookup` defs of

    Just Definition { defLoc, def' = ProcedureDef { procParams }} -> do
      let
        nArgs   = length args
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
                { pname = procName
                , pargs = args'' }}
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
      -- If the procedure is not defined, it's possible that we're
      -- dealing with a recursive call. The information of a procedure
      -- that is being defined is stored temporarily at the
      -- Parser.State `currentProc`.
      currentProcedure <- use currentProc
      case currentProcedure of
        Just (name, pos, types, recursionAllowed)
          | name == procName && recursionAllowed -> do
            let
              nArgs = length args
              nParams = length types
            if nArgs == nParams
              then do
                args' <- foldM (checkType procName pos) (Just Seq.empty) (Seq.zip args types)
                pure $ case args' of
                  Nothing -> Nothing
                  Just pargs -> Just Instruction
                    { instLoc = loc
                    , inst' = ProcedureCall
                      { pname = procName
                      , pargs }}
              else do
                putError from BadProcNumberOfArgs
                  { pName = procName
                  , pPos  = pos
                  , nParams
                  , nArgs }
                pure Nothing
          | name == procName && not recursionAllowed -> do
            putError from . UnknownError $
              "Procedure `" <> unpack procName <> "` cannot call itself \
              \recursively because no Bound and Invariant were given for it."
            pure Nothing
        _ -> do
          t <- hasDTType . toList $ args
          case t of

            GUndef -> do
              putError from $ UndefinedProcedure procName
              pure Nothing

            GFullDataType n t -> do
              fdts <- use fullDataTypes
              let dtName = llvmName n t
              case dtName `Map.lookup` fdts of
                Just Struct{structProcs} -> do
                  let
                    procName' = procName <> pack "-" <> dtName
                    procAst' = L.find (\x -> defName x == procName') structProcs
                  case procAst' of
                    Just procAst -> do
                      let
                        Location(from,_) = defLoc procAst
                        ProcedureDef{procParams} = def' procAst

                      args' <- foldM (checkType procName from)
                        (Just Seq.empty) (Seq.zip args procParams)

                      pure $ case args' of
                        Nothing -> Nothing
                        Just args'' -> Just Instruction
                          { instLoc = loc
                          , inst' = ProcedureCall
                            { pname = procName'
                            , pargs = args'' }}

                    Nothing -> do
                      putError from . UnknownError $
                        "Data Type `" <> unpack (takeWhile (/= '-') dtName) <>
                        "` does not have a procedure called `" <>
                        unpack procName <> "`"
                      return Nothing

                _ -> error "No deberia estar aqui :D (:"


  where
    hasDTType [] = pure GUndef
    hasDTType (Nothing:xs) = hasDTType xs
    hasDTType (Just x:xs) = case expType x of
      fdt@GFullDataType{} -> pure fdt
      dt@GDataType{} -> do
        let Location(from,_) = E.loc x
        putError from . UnknownError $
          "Expression `" <> show x <> "` has an incomplete type " <> show dt
        pure GUndef
      _ -> hasDTType xs


    checkType _ _ _ (Nothing, _) = pure Nothing
    checkType pName pPos acc
      (Just e@Expression { E.loc = Location (from, _), expType, exp'}, (name, pType, mode)) =
        if pType =:= expType
          then case exp' of
            Obj {}
              | mode `elem` [Out, InOut] ->
                pure $ (|> (e, mode)) <$> acc
            _
              | mode == In ->
                pure $ (|> (e, mode)) <$> acc
            _ -> do
              putError from . UnknownError $
                "The parameter `" <> unpack name <> "` has mode " <>
                show mode <> "\n\tbut recived an expression instead \
                \of a variable"
              pure Nothing
          else do
            putError from $
              BadProcedureArgumentType name pName pPos pType expType
            pure Nothing


-- | Parse `skip` instruction.
skip :: Parser (Maybe Instruction)
skip = do
  from <- getPosition
  match TokSkip
  to <- getPosition
  pure . Just $ Instruction (Location (from, to)) Skip
