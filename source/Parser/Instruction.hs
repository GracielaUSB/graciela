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
  , writeln
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
import           AST.Expression         (Expression (..), Object (..))
import           AST.Expression         as E
import           AST.Instruction        (Guard, Instruction (..),
                                         Instruction' (..))
import           AST.Object
import           Entry
import           Error                  as PE
import           Location
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Expression
import           Parser.Monad
import           Parser.Recovery
import           Parser.State
import           Parser.Type
import           SymbolTable
import           Token
import           Type                   (ArgMode (..), Type (..), (=:=))
-------------------------------------------------------------------------------
import           Control.Lens           (use, (%=), (^.))
import           Control.Monad          (foldM, unless, void, when, zipWithM)
import           Control.Monad.Identity (Identity)
import qualified Data.List              as L (any)
import           Data.Monoid            ((<>))
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T (pack, unpack)
import           Debug.Trace
import           Prelude                hiding (lookup)
import           Text.Megaparsec        (between, eitherP, endBy, getPosition,
                                         lookAhead, many, notFollowedBy, sepBy,
                                         sepBy1, try, (<|>))
-------------------------------------------------------------------------------

instruction :: Parser (Maybe Instruction)
instruction = try procedureCall
          <|> try assign
          <|> assertionInst
          <|> abort
          <|> conditional
          <|> free
          <|> new
          <|> random
          <|> reading
          <|> repetition
          <|> skip
          <|> write
          <|> writeln
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


declarationBlock :: Parser (Maybe [Declaration])
declarationBlock = sequence <$> (declaration `endBy` match TokSemicolon)


declarationOrRead :: Parser (Maybe [Either Declaration Instruction])
declarationOrRead = sequence <$> (p `endBy` match TokSemicolon)
  where
    p = do
      line <- eitherP declaration reading
      pure $ case line of
        Left  Nothing  -> Nothing
        Left  (Just l) -> Just (Left l)
        Right Nothing  -> Nothing
        Right (Just l) -> Just (Right l)


-- block :: Parser (Maybe Instruction)
-- block = do
--   from <- getPosition
--   symbolTable %= openScope from
--   match TokOpenBlock
--   decls       <- declarationBlock
--   actions     <- many insts
--   assertions' <- many assertionInst
--   st          <- use symbolTable
--   let actions' = (concat actions) <> assertions'
--   match TokCloseBlock
--   to <- getPosition
--   symbolTable %= closeScope to
--
--   let loc = Location (from, to)
--
--   if null actions
--     then do
--       putError loc EmptyBlock
--       pure Nothing
--   else if L.any (\x -> case x of; Nothing -> True; _ -> False) actions'
--     then pure Nothing
--   else pure $ case (decls, actions') of
--     (Nothing, _) -> Nothing
--     (_, Nothing) -> Nothing
--     (Just ds, Just as) -> Just $ Instruction
--             { instLoc = loc
--             , inst'   = Block
--               { blockST    = st
--               , blockDecs  = ds
--               , blockInsts = actions' }}
--   where
--     insts = do
--       a1   <- many assertionInst
--       inst <- instruction
--       a2   <- many assertionInst
--       do (void . lookAhead $ match TokCloseBlock)
--         <|> (void $ match TokSemicolon)
--       pure $ a1 <> [inst] <> a2


block :: Parser (Maybe Instruction)
block = do
  from <- getPosition
  symbolTable %= openScope from
  match TokOpenBlock

  decls       <- declarationBlock
  actions     <- sequence <$> many instruction
  st          <- use symbolTable -- ??

  match TokCloseBlock
  to <- getPosition
  symbolTable %= closeScope to

  let loc = Location (from, to)

  if null actions
    then do
      putError loc EmptyBlock
      pure Nothing
  else if L.any (\x -> case x of; Nothing -> True; _ -> False) actions'
    then pure Nothing
  else pure $ case (decls, actions') of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just ds, Just as) -> Just $ Instruction
            { instLoc = loc
            , inst'   = Block
              { blockST    = st
              , blockDecs  = ds
              , blockInsts = actions' }}
  where
    insts = do
      a1   <- many assertionInst
      inst <- instruction
      a2   <- many assertionInst
      do (void . lookAhead $ match TokCloseBlock)
        <|> (void $ match TokSemicolon)
      pure $ a1 <> [inst] <> a2


assign :: Parser (Maybe Instruction)
assign = do
  from <- getPosition

  lvals <- expression `sepBy1` match TokComma
  match TokAssign
  exprs <- expression `sepBy1` match TokComma

  to <- getPosition

  let len = length lvals == length exprs

  unless len . putError (Location (from, to)) . UnknownError $
    "La cantidad de lvls es distinta a la de expresiones"


  (correct, lvals') <- checkTypes (zip lvals exprs)

  if correct && len
    then pure $ Instruction (Location(from,to)) (Assign lvals' exprs)
    else pure Nothing

  where
    {- Checks if the left expressions are valid lvals and
       if the assigned expression has the correct type
    -}
    checkTypes :: [(Expression,Expression)] -> Parser (Bool,[Object])
    checkTypes [] = pure (True,[])
    checkTypes (x:xs) = case x of
      (Expression loc1 t1 (Obj o), Expression loc2 t2 _) | notIn o -> do
        if (t1 =:= t2) && (t1 =:= GOneOf [GInt, GFloat, GBool, GChar, GPointer GAny])
          then do
            (c,objs) <- checkTypes xs
            pure (c, o:objs)
          else do
            putError loc1 . UnknownError $
              "No se puede asignar una expresion del tipo `" <>
              show t2 <> "` a una variable del tipo `" <>
              show t1 <> "`"
            (c,objs) <- checkTypes xs
            pure (False,objs)
      (Expression loc1 t1 (Obj o), Expression{}) -> do
        putError loc1 . UnknownError $
          "The variable `" <> show o <> "` cannot be assigned because it has mode In"
        (c,objs) <- checkTypes xs
        pure (False, objs)
      (Expression loc _ _, Expression {}) -> do
        putError loc $ UnknownError
          "No se puede asignar un valor a una expresion"
        (c,objs) <- checkTypes xs
        pure (False, objs)
      _ -> checkTypes xs


random :: Parser (Maybe Instruction)
random = do
  from <- getPosition
  match TokRandom
  expr   <- parens expression
  to   <- getPosition
  let loc = Location (from,to)
  {- Checks if the expression can be assigned -}
  case expr of
    Expression { E.loc, expType, exp' } -> case exp' of
      -- Only int objects can be randomized (maybe char or float too?)
      Obj o | correctType expType && notIn o ->
        pure $ Instruction loc (Random o)
      -- If not, its an expression or a constant (or both).
      _ -> do
        putError loc . UnknownError $
          "No se puede asignar un numero random a una expresion constante"
        pure Nothing
    -- If its a bad expression just pure bad instruction
    -- _ -> pure Nothing
    -- FIXME: improving constant field of Objects
  -- pure Nothing
  where
    correctType = (=:= GOneOf [GInt{-, GFloat, GBool, GChar-}])


-- Parse `write` instruction
write :: Parser (Maybe Instruction)
write = write' False TokWrite

-- Parse `writeln` instruction
writeln :: Parser (Maybe Instruction)
writeln = write' True TokWriteln

write' :: Bool -> Token -> Parser (Maybe Instruction)
write' ln writeToken = do
  from <- getPosition
  match writeToken
  e <- parens $ (string <|> expression)`sepBy1` match TokComma
  to <- getPosition
  let loc = Location(from,to)
  if False `elem` (fmap (\x -> case x of; Expression {} -> True; _ -> False) e)
    then pure Nothing
    else pure $ Instruction loc (Write ln e) -- if ln == True -> writeln, else -> write

-- Parse the read instrucction
reading :: Parser (Maybe Instruction)
reading = do
  from  <- getPosition
  match TokRead
  ids   <- parens $ expression `sepBy1` match TokComma
  res   <- mapM isWritable ids
  let types = fmap fst res
  let objs  = fmap snd res
  -- If any expression has type `GUndef`, pure Nothing
  if GUndef `elem` types
    then do
      to <- getPosition
      let location = Location(from,to)
      when (null ids) . putError location $
          UnknownError "Read function most have at least one argument"
      pure Nothing
    else do
      -- Read instruccion can be followed by the token `with` and a file name.
      -- In that case, save the file name in state's `fileToRead` and
      -- pure the instruction
      match TokWith
      id <- stringLit
      filesToRead %= Set.insert (T.unpack id)
      to <- getPosition
      let location = Location(from,to)
      when (null ids) . putError location $
          UnknownError "Read function most have at least one argument"
      pure $ Instruction
            { instLoc   = location
            , inst' = Read
                { file     = Just id
                , varTypes = types
                , vars     = objs}}
      <|> do
        -- If no token `with` is found, just pure the instruction
        to <- getPosition
        let location = Location(from,to)
        when (null ids) . putError location $
          UnknownError "Read function most have at least one argument"
        pure $ Instruction
              { instLoc   = location
              , inst' = Read
                  { file     = Nothing
                  , varTypes = types
                  , vars     = objs}}

    where
      {- Checks the expression is a variable and if it has a basic type -}
      isWritable :: Expression -> Parser (Type, Object)
      isWritable expr = case expr of
        Expression {E.loc, expType, exp'} ->
          case exp' of
            -- Only objects can be assigned, only if is not a constant an is int (maybe char or float?)
            Obj o -> if correctType expType && notIn o
              then pure (expType, o)
              else do
                putError loc $ BadReadArgumentType expr expType
                pure (GUndef, Nothing)
            -- If not, its an expression or a constant (or both).
            _ -> do
              putError loc $ BadReadArgument expr
              pure (GUndef, Nothing)
          -- FIXME: improving constant field of object
            -- pure (GUndef, Nothing)

        -- If its a bad expression just pure bad instruction
        Nothing -> pure (GUndef, Nothing)
        where
            correctType = (=:= GOneOf [GInt, GFloat, GChar])

new :: Parser (Maybe Instruction)
new  = do
    from <- getPosition
    match TokNew
    id <- parens expression
    to <- getPosition

    let loc = Location(from,to)
    case id of
      Expression _ (GPointer t) (Obj o) ->
        pure $ Instruction loc (New o t)
      _     -> do
        putError loc $ UnknownError "New can only recive pointers"
        pure Nothing


free :: Parser (Maybe Instruction)
free = do
    from <- getPosition
    match TokFree
    id <- parens expression
    to <- getPosition

    let loc = Location(from,to)
    case id of
      Expression _ (GPointer t) (Obj o) ->
        pure $ Instruction loc (Free o t)

      _     -> do
        putError loc $ UnknownError "New can only recive pointers"
        pure Nothing

abort :: Parser (Maybe Instruction)
abort =
    do pos <- getPosition
       match TokAbort
       pure $ Instruction (Location(pos,pos)) Abort

{- Parse guards for both repetition and conditional -}
guard :: Parser (Maybe Guard)
guard = do
  from <- getPosition
  symbolTable %= openScope from
  cond  <- expression
  match TokArrow
  decls       <- declarationBlock
  actions     <- many insts
  assertions' <- many assertionInst
  st          <- use symbolTable
  let actions' = (concat actions) <> assertions'
  to <- getPosition
  symbolTable %= closeScope to
  let loc = Location (from, to)
  when (null actions) $ putError loc EmptyBlock
  pure (cond,decls,actions')
  where
    insts = do
      a1   <- many assertionInst
      inst <- instruction
      a2   <- many assertionInst
      do (void . lookAhead $ match TokFi <|> match TokOd <|> match TokSepGuards)
        <|> (void $ match TokSemicolon)
      pure $ a1 <> [inst] <> a2

conditional ::  Parser (Maybe Instruction)
conditional = do
  from <- getPosition
  match TokIf
  gl <- guard `sepBy` match TokSepGuards
  match TokFi
  to <- getPosition
  pure $ Instruction (Location(from,to)) (Conditional gl)


{- Parse the instruction do .. od -}
repetition :: Parser (Maybe Instruction)
repetition = do
    {- First case: Neither invariant nor bound -}
    from <- getPosition
    try $ match TokDo
    gl <- guard `sepBy` match TokSepGuards
    match TokOd
    to <- getPosition
    let location = Location (from,to)
    putError location NoDoInvariant
    putError location NoDoBound
    pure Nothing
    <|> do
      {- Second case: No invariant -}
      from <- getPosition
      lookAhead $ match TokLeftBound
      bound
      match TokDo
      guard `sepBy` match TokSepGuards
      match TokOd
      to <- getPosition
      let location = Location (from,to)
      putError location NoDoBound
      pure Nothing
    <|> do
      {- Third case: An invariant is at the lookAhead.
         Parse normally and in case of not-}
      lookAhead (match TokLeftInv)
      from   <- getPosition
      inv    <- safeAssertion invariant NoDoInvariant
      bound' <- safeAssertion bound NoDoBound
      match TokDo
      gl <- guard `sepBy` match TokSepGuards
      match TokOd
      to <- getPosition
      pure $ Instruction
          { instLoc = Location(from,to)
          , inst'   = Repeat
            { rguards = gl
            , rinv    = inv
            , rbound  = bound'}}


procedureCall :: Parser (Maybe Instruction)
procedureCall = do
  from <- getPosition
  id   <- identifier
  match TokLeftPar
  args <- expression `sepBy` match TokComma
  match TokRightPar
  st   <- use symbolTable

  to   <- getPosition
  let loc = Location (from,to)

  case id `lookup` st of
    {- Check if the called procedure if defined in the symbol table-}
    Right (Entry _ (Location (pos,_)) (Procedure {_procParams})) -> do
        let nArgs   = length args
        let nParams = length _procParams
        {- Check if the call recived enough arguments-}
        if nArgs /= nParams
          then do
            putError loc BadProcNumberofArgs {
                            pName   = id
                          , pPos    = pos
                          , nParams = nParams
                          , nArgs   = nArgs
                          }
            pure Nothing
        else if nArgs == 0 && nParams == 0
            then pure $ Instruction loc (ProcedureCall id [])
        else do
          {- Now check the arguments types match with the procedure parameter's types-}
          args' <- zipWithM (checkTypes id pos loc) _procParams args
          pure $ Instruction loc (ProcedureCall id args')

    _ -> do
      {- If the procedure is not defined, maybe the current procedure is calling
         itself recursively. The information of a procedure that is being defined is stored
         temporarily at Parser's currentSymbol -}
      currentProcedure <- use currentProc
      case currentProcedure of
        {- If the current symbol match with the call, then check the arguments types
           and pure the proper AST -}
        Just (name, pos, types) | name == id -> do
          let nArgs   = length args
          let nParams = length types
          {- Check if the call recived enough arguments-}
          if nArgs /= nParams
            then do
              putError loc BadProcNumberofArgs {
                              pName   = id
                            , pPos    = pos
                            , nParams = nParams
                            , nArgs   = nArgs
                            }
              pure Nothing
          else if nArgs == 0 && nParams == 0
            then pure $ Instruction loc (ProcedureCall id [])
          else do
            {- Now check the arguments types match with the procedure parameter's types-}
            args' <- zipWithM (checkTypes id pos loc) types args
            pure $ Instruction loc (ProcedureCall id args')
        {- If there is no procedure defined that matchs with the current call, then report the error-}
        Nothing -> do
          putError loc (UndefinedProcedure id)
          pure Nothing
  where
    checkTypes pName pPos loc (name, pType, mode) Just e@Expression {expType, exp'} = do
      if pType == expType
        then case exp' of
          Obj obj | mode == Out || mode == InOut -> pure (e,mode)
          _       | mode == In -> pure (e,mode)
          _ -> do
            putError loc . UnknownError $ "The parameter `" <> T.unpack name <> "` has mode " <>
                 show mode <> "\n\tbut recived an expression instead of a variable"
            pure (e,mode)
        else do
          putError loc $ BadProcedureArgumentType name pName pPos pType expType
          pure (e,mode)
    checkTypes _ _ _ _ Nothing = pure (Nothing,In)


skip :: Parser (Maybe Instruction)
skip =
    do  pos <- getPosition
        match TokSkip
        pure $ Instruction (Location(pos,pos)) Skip
