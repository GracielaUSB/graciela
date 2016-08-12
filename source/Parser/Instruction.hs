{-# LANGUAGE NamedFieldPuns#-}
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
import           AST.Declaration    (Declaration)
import           AST.Expression     (Expression(..),Expression'(..),Object(..))
import           AST.Expression     as E
import           AST.Instruction    (Instruction(..),Instruction'(..),Guard)
import           AST.Object
import           AST.Type           (Type, Type'(..), (=:=), ArgMode(..))
import           Entry
import           Graciela
import           Location
import           Error              as PE
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Expression
import           Parser.Recovery
import           Parser.Token
import           Parser.Type
import           SymbolTable
import           Token
-------------------------------------------------------------------------------
import           Control.Lens           (use, (%=), (^.))
import           Control.Monad          (unless, void, when, foldM)
import           Control.Monad.Identity (Identity)
import qualified Data.List              as L (any)
import           Data.Monoid            ((<>))
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T (unpack, pack)
import           Text.Megaparsec        (sepBy, endBy, sepBy1, notFollowedBy,
                                         getPosition, try, between, (<|>),
                                         lookAhead, eitherP, many )
import           Prelude                hiding (lookup)
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
  (constantDeclaration <|> variableDeclaration) `endBy` match TokSemicolon

declarationOrRead :: Graciela [Either Declaration Instruction]
declarationOrRead = 
  let declaration = constantDeclaration <|> variableDeclaration
  in eitherP declaration reading `endBy` match TokSemicolon

block :: Graciela Instruction
block = do
  from <- getPosition
  symbolTable %= openScope from
  match TokOpenBlock

  decls       <- declarationBlock
  actions     <- insts `sepBy` match TokSemicolon
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
      return $ a1 <> [inst] <> a2


assign :: Graciela Instruction
assign = do
  from <- getPosition

  lvals <- expression `sepBy1` match TokComma
  withRecovery TokAssign
  exprs <- expression `sepBy1` match TokComma
  
  to <- getPosition
  
  let len = length lvals == length exprs
  
  unless len . syntaxError $ CustomError
    "La cantidad de lvls es distinta a la de expresiones"
    (Location (from, to))
  
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
      (Expression loc1 t1 constant (Obj o), Expression loc2 t2 _ _) 
          | constant == False -> do 
        if t1 =:= GOneOf [GInt, GFloat, GBool, GChar, GPointer GAny]
          then do
            (c,objs) <- checkTypes xs
            return (c, o:objs)
          else do
            syntaxError $ CustomError
                ("No se puede asignar una expresion del tipo `" <>
                  show t2 <> "` a una variable del tipo `" <>
                  show t1 <> "`") loc1
            (c,objs) <- checkTypes xs
            return (False,objs)
      (Expression loc _ _ _, Expression {}) -> do
        syntaxError $ CustomError
          "No se puede asignar un valor a una expresion"
          loc
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
    Expression {E.loc, expType, constant, exp'} -> case exp' of
      -- Only objects can be assigned, only if is not a constant an is int (maybe char or float?)
      Obj o | correctType expType && not constant ->
        return $ Instruction loc (Random o)
      -- If not, its an expression or a constant (or both).
      _ -> do
        genCustomError "No se puede asignar un numero random a una expresion constante"
        return $ BadInstruction loc
    -- If its a bad expression just return bad instruction
    _ -> return $ BadInstruction loc 
  where 
    correctType = (=:= GOneOf [GInt{-, GFloat, GBool, GChar-}])

write :: Graciela Instruction
write = write' False TokWrite

writeln :: Graciela Instruction
writeln = write' True TokWriteln

write' :: Bool -> Token -> Graciela Instruction
write' ln writeToken = do
  from <- getPosition
  match writeToken
  e <- between (match TokLeftPar) (match TokRightPar) expression
  to <- getPosition
  let loc = Location(from,to)
  case e of
    Expression {} ->
      return $ Instruction loc (Write ln e)
    _ -> return $ BadInstruction loc

-- | Se encarga del parseo de la lectura de variables
reading :: Graciela Instruction
reading = do
  from  <- getPosition
  match TokRead
  match TokLeftPar
  ids   <- expression `sepBy` match TokComma
  match TokRightPar
  res   <- mapM isWritable ids
  let types = fmap fst res
  let objs  = fmap snd res
  if GUndef `elem` types
    then do
      to <- getPosition
      return $ BadInstruction (Location(from,to))
    else
      try $ do
          match TokWith
          id <- stringLit
          filesToRead %= Set.insert (T.unpack id)
          to <- getPosition
          return $ Instruction 
                { instLoc   = (Location(from,to))
                , inst' = Read 
                    { file     = Just id
                    , varTypes = types
                    , vars     = objs}}
        <|> do
          to <- getPosition
          return $ Instruction 
                { instLoc   = (Location(from,to))
                , inst' = Read 
                    { file     = Nothing
                    , varTypes = types
                    , vars     = objs}}

    where
      {- Checks the expression is a variable and if it has a basic type -}
      isWritable :: Expression -> Graciela (Type, Object)
      isWritable expr = case expr of 
        Expression {E.loc, expType, constant, exp'} -> case exp' of
          -- Only objects can be assigned, only if is not a constant an is int (maybe char or float?)
          Obj o | correctType expType && not constant ->
            return (expType, o)
          -- If not, its an expression or a constant (or both).
          _ -> do
            putError loc $ BadReadArgumentType (T.pack (show expr)) expType
            return (GUndef, BadObject loc)
        -- If its a bad expression just return bad instruction
        BadExpression loc -> return (GUndef, BadObject loc)
        where 
            correctType = (=:= GOneOf [GInt, GFloat, GBool, GChar])

new :: Graciela Instruction
new  = do
    from <- getPosition
    match TokNew
    match TokLeftPar
    id <- identifier
    match TokRightPar
    to <- getPosition

    let loc = Location(from,to)
    st <- use symbolTable
    case id `lookup` st of
      Right entry -> case _info entry of
        Var (GPointer t) _ ->
          return $ Instruction loc (New id)
        _              -> do
          genCustomError "New solo recibe apuntadores"
          return $ BadInstruction loc
      Left _ -> do
        genCustomError ("La variable `" <> T.unpack id <> "` No existe.")
        return $ BadInstruction loc


free :: Graciela Instruction
free = do
    from <- getPosition
    match TokFree
    match TokLeftPar
    id <- identifier
    match TokRightPar
    to <- getPosition

    let loc = Location(from,to)
    st <- use symbolTable
    case id `lookup` st of
      Right entry -> case _info entry of
        Var (GPointer t) _ ->
          return $ Instruction loc (Free id)
        _              -> do
          genCustomError "Free solo recibe apuntadores"
          return $ BadInstruction loc
      Left _ -> do
        genCustomError ("La variable `" <> T.unpack id <> "` No existe.")
        return $ BadInstruction loc

abort :: Graciela Instruction
abort =
    do pos <- getPosition
       match TokAbort
       return $ Instruction (Location(pos,pos)) Abort

{- Parse guards for both repetition and conditional -}
guard :: Graciela Guard
guard = do
  from  <- getPosition
  cond  <- expression
  match TokArrow
  symbolTable %= openScope from
  inst  <- instruction
  to    <- getPosition
  symbolTable %= closeScope to
  return (cond,inst)


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
            return $ BadInstruction loc
        else if nArgs == 0 && nParams == 0
            then return $ Instruction loc (ProcedureCall id [])
        else do 
          {- Now check the arguments types match with the procedure parameter's types-}
          argumentsOk <- foldM (checkTypes id pos loc) False $ zip _procParams args
          if argumentsOk 
            then return $ Instruction loc (ProcedureCall id args)
            else return $ BadInstruction loc
    
    _ -> do
      {- If the procedure is not defined, maybe the current block is a procedure calling 
         itself, recursively. The information of a procedure that is beign defined is store 
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
            argumentsOk <- foldM (checkTypes id pos loc) False $ zip types args
            if argumentsOk 
              then return $ Instruction loc (ProcedureCall id args)
              else return $ BadInstruction loc
        {- If there is no procedure defined that matchs with the current call, then report the error-}
        Nothing -> do 
          putError loc (UndefinedProcedure id)
          return $ BadInstruction loc
  where 
    checkTypes pName pPos loc ok ((name, pType), Expression {expType}) = do 
      if pType == expType 
        then return ok
        else do
          putError loc $ BadProcedureArgumentType name pName pPos pType expType
          return False
    checkTypes _ _ _ _ _ = return False


skip :: Graciela Instruction
skip =
    do  pos <- getPosition
        match TokSkip
        return $ Instruction (Location(pos,pos)) Skip
