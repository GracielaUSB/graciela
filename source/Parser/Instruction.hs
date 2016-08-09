{-# LANGUAGE NamedFieldPuns#-}
module Parser.Instruction
  ( instruction
  , declarationBlock
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
import           AST.Expression
import           AST.Instruction
import           AST.Object
import           Entry
import           Graciela
import           Location
import           MyParseError           as PE
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Expression
import           Parser.Token
import           Parser.Type
import           SymbolTable
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Lens           (use, (%=), (^.))
import           Control.Monad          (unless, void, when, foldM)
import           Control.Monad.Identity (Identity)
import qualified Data.List              as L (any)
import           Data.Monoid            ((<>))
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T (unpack)
import           Text.Megaparsec        (sepBy, endBy, sepBy1, notFollowedBy,
                                         getPosition, try, between, (<|>),
                                         lookAhead)
import           Prelude                hiding (lookup)
-------------------------------------------------------------------------------

instruction :: Graciela Instruction
instruction = try procedureCall
          <|> try assign
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

declarationBlock :: Graciela [Instruction]
declarationBlock =
  (constantDeclaration <|> variableDeclaration) `endBy` match TokSemicolon

block :: Graciela Instruction
block = do
  from <- getPosition
  symbolTable %= openScope from
  match TokOpenBlock

  decls   <- declarationBlock
  actions <- instruction `sepBy` match TokSemicolon
  st      <- use symbolTable

  withRecovery TokCloseBlock
  to <- getPosition
  symbolTable %= closeScope to

  let loc = Location (from, to)

  if L.any (\x -> case x of; NoInstruction _ -> True; _ -> False) actions
    then return $ NoInstruction loc
    else return $ Instruction loc (Block st decls actions)

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
    else return $ NoInstruction (Location(from,from))

  where
    {- Checks if the left expressions are valid lvals and
       if the assigned expression has the correct type
    -}
    checkTypes :: [(Expression,Expression)] -> Graciela (Bool,[Object])
    checkTypes [] = return (True,[])
    checkTypes (x:xs) = case x of 
      (Expression loc1 t1 (Obj o), Expression loc2 t2 _) -> do 
        if t1 == t2 && t1 =:= GOneOf [GInt, GFloat, GBool, GChar, GPointer GAny]
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
      (Expression loc _ _, _) -> do
        syntaxError $ CustomError
          "No se puede asignar un valor a una expresion"
          loc
        (c,objs) <- checkTypes xs
        return (False, objs)


random :: Graciela Instruction
random = do
  from <- getPosition
  match TokRandom
  match TokLeftPar
  id   <- identifier
  match TokRightPar
  to   <- getPosition
  let loc = Location (from,to)

  writable  <- isWritable id
  if writable
    then return $ Instruction loc (Random id)
    else return $ NoInstruction loc

  where
    {- Checks if an entry is a variable or an In/InOut argument -}
    isWritable :: Text -> Graciela Bool
    isWritable id = do
      st <- use symbolTable
      case id `lookup` st of
        Right entry -> case _info entry of
            Var {} -> return True
            Argument mode _ | mode == In || mode == InOut ->
              return True
            _ -> return False
        _ -> do
          genCustomError ("La variable `" <> T.unpack id <> "` No existe.")
          return False




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
    _ -> return $ NoInstruction loc

-- | Se encarga del parseo de la lectura de variables
reading :: Graciela Instruction
reading = do
  from <- getPosition
  match TokRead
  match TokLeftPar
  ids <- identifierAndLoc `sepBy` match TokComma
  match TokRightPar
  types <- mapM isWritable ids
  if GError `elem` types
    then do
      to <- getPosition
      return $ NoInstruction (Location(from,to))
    else
      try $ do
          match TokWith
          id <- stringLit
          filesToRead %= Set.insert (T.unpack id)
          to <- getPosition
          return $ Instruction (Location(from,to)) (Read (Just id) types ids)
        <|> do
          to <- getPosition
          return $ Instruction (Location(from,to)) (Read Nothing types ids)

    where
      {- Checks if an entry is a variable or an In/InOut argument and
         checks if it has a basic type -}
      isWritable :: (Text,Location) -> Graciela Type
      isWritable (id,loc) = do
        st <- use symbolTable
        case id `lookup` st of
          Right entry -> case _info entry of
              Var t _ -> if t =:= GOneOf [GInt, GFloat, GBool, GChar, GPointer GAny]
                then return t
                else do
                  putError loc $ InvalidReadArgumentType id t
                  return GError
              Argument mode t | mode == In || mode == InOut  ->
                if t =:= GOneOf [GInt, GFloat, GBool, GChar, GPointer GAny]
                then return t
                else do
                  putError loc $ InvalidReadArgumentType id t
                  return GError
              _ -> do
                putError loc $ InvalidReadArgument id
                return GError
          _ -> do
            putError loc $ UndefinedProcedure id
            return GError


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
          return $ NoInstruction loc
      Left _ -> do
        genCustomError ("La variable `" <> T.unpack id <> "` No existe.")
        return $ NoInstruction loc


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
          return $ NoInstruction loc
      Left _ -> do
        genCustomError ("La variable `" <> T.unpack id <> "` No existe.")
        return $ NoInstruction loc

abort :: Graciela Instruction
abort =
    do pos <- getPosition
       match TokAbort
       return $ Instruction (Location(pos,pos)) Abort

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



repetition :: Graciela Instruction
repetition = do
  from   <- getPosition
  inv    <- invariant
  bound' <- bound    
  withRecovery TokDo
  gl <- guard `sepBy` match TokSepGuards
  to <- getPosition
  withRecovery TokOd
  return $ Instruction (Location(from,to)) (Repeat gl inv bound')

procedureCall :: Graciela Instruction
procedureCall = do
  from <- getPosition
  id   <- identifier
  lookAhead (match TokLeftPar)

  withRecovery TokLeftPar
  args <- expression `sepBy` match TokComma
  withRecovery TokRightPar
  st   <- use symbolTable
  
  to   <- getPosition
  let loc = Location (from,to)
  
  case id `lookup` st of
    {- Check if the called procedure if defined in the symbol table-}
    Right (Entry _ (Location (pos,_)) (Procedure {_procArgs})) -> do
        {- Now check the arguments types match with the procedure parameter's types-}
        argumentsOk <- foldM (checkTypes id pos loc) False $ zip _procArgs (fmap expType args)
        if argumentsOk 
          then return $ Instruction loc (ProcedureCall id args)
          else return $ NoInstruction loc
    
    _ -> do
      {- If the procedure is not defined, maybe the current block is a procedure calling 
         itself, recursively. The information of a procedure that is beign defined is store 
         temporaly at Graciela's currentSymbol -}
      currentProcedure <- use currentProc
      case currentProcedure of 
        {- If the current symbol match with the call, then check the arguments types 
           and return the proper AST -}
        Just (name, pos, types) | name == id -> do
          argumentsOk <- foldM (checkTypes id pos loc) False $ zip types (fmap expType args)
          if argumentsOk 
            then return $ Instruction   loc (ProcedureCall id args)
            else return $ NoInstruction loc
        {- If there is no procedure defined that matchs with the current call, then report the error-}
        Nothing -> do 
          putError loc (UndefinedProcedure id)
          return $ NoInstruction loc
  where 
    checkTypes pName pPos loc ok ((name, pType), aType) = do 
      if pType == aType 
        then return ok
        else do
          putError loc $ InvalidProcedureArgumentType name pName pPos pType aType
          return False


skip :: Graciela Instruction
skip =
    do  pos <- getPosition
        match TokSkip
        return $ Instruction (Location(pos,pos)) Skip
