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
import           AST.Instruction
import           AST.Expression
import           AST.Object
import           Graciela
import           MyParseError           as PE
import           Parser.Assertion
import           Parser.Declaration
import           Parser.Expression
import           Parser.Token
import           Parser.Type
import           Parser.State
import           SymbolTable
import           Entry
import           Location
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Applicative    (liftA2, liftA3)
import           Control.Lens           (use, (^.), (%=))
import           Control.Monad          (liftM4, void, when)
import           Control.Monad.Identity (Identity)
import qualified Data.Text              as T (Text,unpack)
import qualified Data.Set               as Set
import qualified Data.List              as L (any)
import           Text.Megaparsec        hiding (Token)
import           Prelude                hiding (lookup)
-------------------------------------------------------------------------------

instruction :: Graciela Instruction
instruction =
        abort
    <|> try assign
    <|> block
    <|> conditional
    <|> free
    -- <|> functionCallOrAssign
    <|> new
    <|> random
    <|> repetition
    <|> skip
    <|> write
    <|> writeln

declarationBlock :: Graciela [Instruction]
declarationBlock = 
  (constantDeclaration <|> variableDeclaration <|> reading) 
  `sepBy` (match TokSemicolon)

block :: Graciela Instruction
block = do 
  from <- getPosition
  symbolTable %= openScope from
  match TokOpenBlock
  decls   <- declarationBlock
  actions <- instruction `sepBy` (match TokSemicolon)
  st      <- use symbolTable
  match TokCloseBlock
  to <- getPosition
  symbolTable %= closeScope to
  let loc = (Location(from,to))
  if L.any (\x -> case x of; NoInstruction _ -> True; _ -> False) actions
    then return $ NoInstruction loc
    else return $ Instruction loc (Block st decls actions)

assign :: Graciela Instruction
assign = do 
  from <- getPosition
  lvals <- expression `sepBy` match TokComma
  match TokAssign
  exprs <- expression `sepBy` match TokComma
  to <- getPosition
  let len = length lvals == length exprs
  when (not len) (syntaxError $ CustomError 
          ("La cantidad de lvls es distinta a la de expresiones") 
          (Location(from,to)))
  (correct, lvals') <- checkTypes (zip lvals exprs) 
  if correct && len
    then return $ Instruction (Location(from,to)) (Assign lvals' exprs)
    else return $ NoInstruction (Location(from,from))

  where 
    {- Checks if the left expressions are valid lvals and 
       if the assigned expression has the correct type
    -}
    checkTypes :: [(Expression,Expression)] -> Graciela (Bool,[Object])
    checks [] = (True,[])
    checkTypes (x:xs) = case x of 
      (Expression loc1 t1 (Obj o), Expression loc2 t2 _) -> do 
        if t1 == t2 && t1 =:= GOneOf [GInt, GFloat, GBool, GChar, GPointer GAny]
          then do 
            (c,objs) <- checkTypes xs
            return $ (c,o:objs)
          else do
            syntaxError $ CustomError 
                ("No se puede asignar una expresion del tipo `" ++ 
                  show t2 ++ "` a una variable del tipo `" ++
                  show t1 ++ "`") loc1
            (c,objs) <- checkTypes xs
            return (False,objs)
      (Expression loc _ _, _) -> do 
        syntaxError $ CustomError 
                ("No se puede asignar un valor a una expresion") 
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
    isWritable :: T.Text -> Graciela Bool
    isWritable id = do 
      st <- use symbolTable
      case id `lookup` st of
        Right entry -> case _info entry of 
            Var {} -> return True
            Argument mode _ | mode == In || mode == InOut -> 
              return True
            _ -> return False
        _ -> do
          genCustomError ("La variable `" ++ T.unpack id ++ "` No existe.")
          return False

guard :: Graciela Guard
guard = do 
  from  <- getPosition
  cond  <- expression
  match TokArrow
  inst  <- instruction
  to    <- getPosition
  return (cond,inst)


write :: Graciela Instruction
write = write' False

writeln :: Graciela Instruction
writeln = write' True

write' :: Bool -> Graciela Instruction
write' ln = do
  from <- getPosition
  match TokWriteln
  e <- between (match TokLeftPar) (match TokRightPar) expression
  to <- getPosition
  let loc = Location(from,to)
  case e of 
    Expression {} ->
      return $ Instruction loc (Write ln e) 
    _ -> return $ NoInstruction loc
             

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
          genCustomError ("New solo recibe apuntadores")
          return $ NoInstruction loc
      Left _ -> do
        genCustomError ("La variable `" ++ T.unpack id ++ "` No existe.")
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
          genCustomError ("Free solo recibe apuntadores")
          return $ NoInstruction loc
      Left _ -> do
        genCustomError ("La variable `" ++ T.unpack id ++ "` No existe.")
        return $ NoInstruction loc

abort :: Graciela Instruction
abort =
    do pos <- getPosition
       match TokAbort
       return $ Instruction (Location(pos,pos)) Abort


conditional ::  Graciela Instruction
conditional = do 
  from <- getPosition
  match TokIf
  gl <- many guard
  match TokFi
  to <- getPosition
  return $ Instruction (Location(from,to)) (Conditional gl)
  
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
    else do 
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
      isWritable :: (T.Text,Location) -> Graciela Type
      isWritable (id,_) = do 
        st <- use symbolTable
        case id `lookup` st of
          Right entry -> case _info entry of 
              Var t _ -> if t `elem` [GBool,GChar,GFloat,GInt]
                then return t
                else do
                  genCustomError ("La variable `" ++ T.unpack id ++ "` no es de un tipo basico.")
                  return GError
              Argument mode t | mode == In || mode == InOut  -> 
                if t `elem` [GBool,GChar,GFloat,GInt]
                then return t
                else do
                  genCustomError ("La variable `" ++ T.unpack id ++ "` no es de un tipo basico.")
                  return GError
              _ -> do 
                genCustomError ("el argumento `" ++ T.unpack id ++ "` no es una variable valida")
                return GError
          _ -> do
            genCustomError ("La variable `" ++ T.unpack id ++ "` No existe.")
            return GError

repetition :: Graciela Instruction
repetition = do 
  from   <- getPosition
  inv    <- invariant
  bound' <- bound    
  match TokDo
  gl <- guard `sepBy` (match TokSepGuards)
  to <- getPosition
  match TokOd
  return $ Instruction (Location(from,to)) (Repeat gl inv bound')
     

skip :: Graciela Instruction
skip =
    do  pos <- getPosition
        match TokSkip
        return $ Instruction (Location(pos,pos)) Skip


    