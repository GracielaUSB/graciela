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
import           AST.Definition         (Definition(..), Definition'(..))
import           AST.Expression         (Expression (..), Object (..))
import           AST.Expression         as E
import           AST.Instruction        (Guard, Instruction (..),
                                         Instruction' (..))
import           AST.Object
import           Entry
import           Error                  as PE
import           Graciela
import           Location
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Expression
import           Parser.Recovery
import           Parser.Token
import           Parser.Type
import           SymbolTable
import           Token
import           Type                   (ArgMode (..), Type (..), (=:=))
-------------------------------------------------------------------------------
import           Control.Lens           (use, (%=), (^.))
import           Control.Monad          (foldM, zipWithM, unless, void, when)
import           Control.Monad.Identity (Identity)
import qualified Data.List              as L (any)
import           Data.Monoid            ((<>))
import           Data.Maybe             (fromJust)
import           Data.Map               as Map (lookup)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T (pack, unpack)
import           Prelude                hiding (lookup)
import           Text.Megaparsec        (between, eitherP, endBy, getPosition,
                                         lookAhead, many, notFollowedBy, sepBy,
                                         sepBy1, try, (<|>))
import     Debug.Trace
-------------------------------------------------------------------------------

instruction :: Graciela Instruction
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

assertionInst :: Graciela Instruction
assertionInst = do
  from <- getPosition
  expr <- assertion
  to   <- getPosition
  return $ Instruction
    { instLoc = Location(from,to)
    , inst'   = Assertion expr
    }


declarationBlock :: Graciela [Declaration]
declarationBlock =
  declaration `endBy` match TokSemicolon

declarationOrRead :: Graciela [Either Declaration Instruction]
declarationOrRead = eitherP declaration reading `endBy` match TokSemicolon

block :: Graciela Instruction
block = do
  from <- getPosition
  symbolTable %= openScope from
  match TokOpenBlock
  decls       <- declarationBlock
  actions     <- many insts
  assertions' <- many assertionInst
  st          <- use symbolTable
  let actions' = (concat actions) <> assertions'
  withRecovery TokCloseBlock
  to <- getPosition
  symbolTable %= closeScope to

  let loc = Location (from, to)

  if null actions
    then do
      putError loc EmptyBlock
      return $ BadInstruction loc
  else if L.any (\x -> case x of; BadInstruction _ -> True; _ -> False) actions'
    then return $ BadInstruction loc
  else return $ Instruction
            { instLoc = loc
            , inst'   = Block
              { blockST    = st
              , blockDecs  = decls
              , blockInsts = actions'}}
  where
    insts = do
      a1   <- many assertionInst
      inst <- instruction
      a2   <- many assertionInst
      do (void $ lookAhead $ match TokCloseBlock)
        <|> (void $ withRecovery TokSemicolon)
      return $ a1 <> [inst] <> a2


assign :: Graciela Instruction
assign = do
  from <- getPosition

  lvals <- expression `sepBy1` match TokComma
  withRecovery TokAssign
  exprs <- expression `sepBy1` match TokComma

  to <- getPosition

  let len = length lvals == length exprs

  unless len . putError (Location (from, to)) $ UnknownError $
    "La cantidad de lvls es distinta a la de expresiones"
    

  (correct, lvals') <- checkTypes (zip lvals exprs)

  if correct && len
    then return $ Instruction (Location(from,to)) (Assign lvals' exprs)
    else return $ BadInstruction (Location(from,from))

  where
    {- Checks if the left expressions are valid lvals and
       if the assigned expression has the correct type
    -}
    checkTypes :: [(Expression,Expression)] -> Graciela (Bool,[Object])
    checkTypes [] = return (True,[])
    checkTypes (x:xs) = case x of
      (Expression loc1 t1 (Obj o), Expression loc2 t2 _) | objMode o /= Just In -> do
        if (t1 =:= t2) && (t1 =:= GOneOf [GInt, GFloat, GBool, GChar, GPointer GAny])
          then do
            (c,objs) <- checkTypes xs
            return (c, o:objs)
          else do
            putError loc1 $ UnknownError $
                ("No se puede asignar una expresion del tipo `" <>
                  show t2 <> "` a una variable del tipo `" <>
                  show t1 <> "`") 
            (c,objs) <- checkTypes xs
            return (False,objs)
      (Expression loc1 t1 (Obj o), Expression{}) -> do
        putError loc1 $ UnknownError $
          "The variable `" <> show o <> "` cannot be assigned because it has mode In"
        (c,objs) <- checkTypes xs
        return (False, objs)
      (Expression loc _ _, Expression {}) -> do

        putError loc $ UnknownError $
          "No se puede asignar un valor a una expresion"
        (c,objs) <- checkTypes xs
        return (False, objs)
      _ -> checkTypes xs


random :: Graciela Instruction
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
      Obj o | objMode o /= Just In && correctType expType ->
        return $ Instruction loc (Random o)
      -- If not, its an expression or a constant (or both).
      _ -> do
        putError loc $ UnknownError $
          "No se puede asignar un numero random a una expresion constante"
        return $ BadInstruction loc
    -- If its a bad expression just return bad instruction
    -- _ -> return $ BadInstruction loc
    -- FIXME: improving constant field of Objects
  -- return $ BadInstruction loc
  where
    correctType = (=:= GOneOf [GInt{-, GFloat, GBool, GChar-}])


-- Parse `write` instruction
write :: Graciela Instruction
write = write' False TokWrite

-- Parse `writeln` instruction
writeln :: Graciela Instruction
writeln = write' True TokWriteln

write' :: Bool -> Token -> Graciela Instruction
write' ln writeToken = do
  from <- getPosition
  match writeToken
  e <- parens $ (string <|> expression)`sepBy1` match TokComma
  to <- getPosition
  let loc = Location(from,to)
  if False `elem` (fmap (\x -> case x of; Expression {} -> True; _ -> False) e)
    then return $ BadInstruction loc
    else return $ Instruction loc (Write ln e) -- if ln == True -> writeln, else -> write

-- Parse the read instrucction
reading :: Graciela Instruction
reading = do
  from  <- getPosition
  match TokRead
  ids   <- parens $ expression `sepBy1` match TokComma
  res   <- mapM isWritable ids
  let types = fmap fst res
  let objs  = fmap snd res
  -- If any expression has type `GUndef`, return BadInstruction
  if GUndef `elem` types
    then do
      to <- getPosition
      let location = Location(from,to)
      when (null ids) $ putError location $ 
          UnknownError "Read function most have at least one argument"
      return $ BadInstruction location
    else do
      -- Read instruccion can be followed by the token `with` and a file name.
      -- In that case, save the file name in state's `fileToRead` and
      -- return the instruction
      match TokWith
      id <- stringLit
      filesToRead %= Set.insert (T.unpack id)
      to <- getPosition
      let location = Location(from,to)
      when (null ids) $ putError location $ 
          UnknownError "Read function most have at least one argument"
      return $ Instruction
            { instLoc   = location
            , inst' = Read
                { file     = Just id
                , varTypes = types
                , vars     = objs}}
      <|> do
        -- If no token `with` is found, just return the instruction
        to <- getPosition
        let location = Location(from,to)
        when (null ids) $ putError location $ 
          UnknownError "Read function most have at least one argument"
        return $ Instruction
              { instLoc   = location
              , inst' = Read
                  { file     = Nothing
                  , varTypes = types
                  , vars     = objs}}

    where
      {- Checks the expression is a variable and if it has a basic type -}
      isWritable :: Expression -> Graciela (Type, Object)
      isWritable expr = case expr of
        Expression {E.loc, expType, exp'} ->
          case exp' of
            -- Only objects can be assigned, only if is not a constant an is int (maybe char or float?)
            Obj o | objMode o /= Just In -> if correctType expType 
              then return (expType, o)
              else do
                putError loc $ BadReadArgumentType expr expType
                return (GUndef, BadObject loc)
            Obj o -> do
              putError loc $ UnknownError $ "The argument `" <> show o <> "` has mode " <>
                  show (fromJust $ objMode o) <> " and cannot be read"
              return (GUndef, BadObject loc)
            -- If not, its an expression or a constant (or both).
            _ -> do
              putError loc $ BadReadArgument expr
              return (GUndef, BadObject loc)
          -- FIXME: improving constant field of object
            -- return (GUndef, BadObject loc)

        -- If its a bad expression just return bad instruction
        BadExpression loc -> return (GUndef, BadObject loc)
        where
            correctType = (=:= GOneOf [GInt, GFloat, GChar])

new :: Graciela Instruction
new  = do
    from <- getPosition
    match TokNew
    id <- parens expression
    to <- getPosition

    let loc = Location(from,to)
    case id of
      Expression _ (GPointer t) (Obj o) ->
        return $ Instruction loc (New o t)
      _     -> do
        putError loc $ UnknownError "New can only recive pointers"
        return $ BadInstruction loc


free :: Graciela Instruction
free = do
    from <- getPosition
    match TokFree
    id <- parens expression
    to <- getPosition

    let loc = Location(from,to)
    case id of
      Expression _ (GPointer t) (Obj o) ->
        return $ Instruction loc (Free o t)
      
      _     -> do
        putError loc $ UnknownError "New can only recive pointers"
        return $ BadInstruction loc

abort :: Graciela Instruction
abort =
    do pos <- getPosition
       match TokAbort
       return $ Instruction (Location(pos,pos)) Abort

{- Parse guards for both repetition and conditional -}
guard :: Graciela Guard
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
  return (cond,decls,actions')
  where
    insts = do
      a1   <- many assertionInst
      inst <- instruction
      a2   <- many assertionInst
      do (void $ lookAhead $ match TokFi <|> match TokOd <|> match TokSepGuards)
        <|> (void $ withRecovery TokSemicolon)
      return $ a1 <> [inst] <> a2

conditional ::  Graciela Instruction
conditional = do
  from <- getPosition
  match TokIf
  gl <- guard `sepBy` match TokSepGuards
  withRecovery TokFi
  to <- getPosition
  return $ Instruction (Location(from,to)) (Conditional gl)


{- Parse the instruction do .. od -}
repetition :: Graciela Instruction
repetition = do
    {- First case: Neither invariant nor bound -}
    from <- getPosition
    try $ match TokDo
    gl <- guard `sepBy` match TokSepGuards
    withRecovery TokOd
    to <- getPosition
    let location = Location (from,to)
    putError location NoDoInvariant
    putError location NoDoBound
    return $ BadInstruction location
    <|> do
      {- Second case: No invariant -}
      from <- getPosition
      lookAhead $ match TokLeftBound
      bound
      withRecovery TokDo
      guard `sepBy` match TokSepGuards
      withRecovery TokOd
      to <- getPosition
      let location = Location (from,to)
      putError location NoDoBound
      return $ BadInstruction location
    <|> do
      {- Third case: An invariant is at the lookAhead.
         Parse normally and in case of not-}
      lookAhead (match TokLeftInv)
      from   <- getPosition
      inv    <- safeAssertion invariant NoDoInvariant
      bound' <- safeAssertion bound NoDoBound
      withRecovery TokDo
      gl <- guard `sepBy` match TokSepGuards
      withRecovery TokOd
      to <- getPosition
      return $ Instruction
          { instLoc = Location(from,to)
          , inst'   = Repeat
            { rguards = gl
            , rinv    = inv
            , rbound  = bound'}}


procedureCall :: Graciela Instruction
procedureCall = do
  from <- getPosition
  id   <- identifier
  match TokLeftPar
  args <- expression `sepBy` match TokComma
  withRecovery TokRightPar
  defs   <- use definitions

  to   <- getPosition
  let loc = Location (from,to)

  case id `Map.lookup` defs of
    {- Check if the called procedure if defined in the symbol table-}
    Just (Definition (Location(pos,_)) _ _ _ ProcedureDef {params}) -> do
        let nArgs   = length args
        let nParams = length params
        {- Check if the call recived enough arguments-}
        if nArgs /= nParams
          then do
            putError loc BadProcNumberofArgs {
                            pName   = id
                          , pPos    = pos
                          , nParams = nParams
                          , nArgs   = nArgs
                          }
            return $ BadInstruction loc
        else if nArgs == 0 && nParams == 0
            then return $ Instruction loc (ProcedureCall id [])
        else do
          {- Now check the arguments types match with the procedure parameter's types-}
          args' <- zipWithM (checkTypes id pos loc) params args
          return $ Instruction loc (ProcedureCall id args')

    _ -> do
      {- If the procedure is not defined, maybe the current procedure is calling
         itself recursively. The information of a procedure that is beign defined is store
         temporaly at Graciela's currentSymbol -}
      currentProcedure <- use currentProc
      case currentProcedure of
        {- If the current symbol match with the call, then check the arguments types
           and return the proper AST -}
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
              return $ BadInstruction loc
          else if nArgs == 0 && nParams == 0
            then return $ Instruction loc (ProcedureCall id [])
          else do
            {- Now check the arguments types match with the procedure parameter's types-}
            args' <- zipWithM (checkTypes id pos loc) types args
            return $ Instruction loc (ProcedureCall id args')
        {- If there is no procedure defined that matchs with the current call, then report the error-}
        _ -> do
          putError loc (UndefinedProcedure id)
          return $ BadInstruction loc
  where
    checkTypes pName pPos loc (name, pType, mode) e@Expression {expType, exp'} = do
      if pType == expType
        then case exp' of
          -- An object In cannot be passed as Out or InOut argument
          Obj obj | objMode obj /= Just In && (mode == Out || mode == InOut) -> return (e,mode)
          -- An object Out cannot be passed as In argument
          Obj obj | mode == In && objMode obj /= Just Out -> return (e,mode)
          -- Else, error
          Obj obj | objMode obj /= Nothing   -> do
            putError loc $ UnknownError $
                 "The parameter `" <> T.unpack name <> "` of the procedure `" <>
                 T.unpack pName <> "` " <> showPos' pPos <> "\n\thas mode " <>
                 show mode <> " but recived a variable with mode " <>
                 show (fromJust $ objMode obj) <> " as argument"
            return (e,mode)
          _       | mode == In               -> return (e,mode)

          _ -> do 
            putError loc $ UnknownError $ "The parameter `" <> T.unpack name <> "` has mode " <>
                 show mode <> "\n\tbut recived an expression instead of a variable"
            return (e,mode)
        else do
          putError loc $ BadProcedureArgumentType name pName pPos pType expType
          return (e,mode)
    checkTypes _ _ _ _ e@BadExpression{} = return (e,In)


skip :: Graciela Instruction
skip =
    do  pos <- getPosition
        match TokSkip
        return $ Instruction (Location(pos,pos)) Skip
