{-|
Module      : Declarations
Description : Parseo y almacenamiento de las declaraciones
Copyright   : Graciela

Se encuentra todo lo referente al almacenamiento de las variables
en la tabla de simbolos, mientras se esta realizando el parser.
-}
module Parser.Declaration where

-------------------------------------------------------------------------------
import           AST.Instruction
import           AST.Expression
import           AST.Object
import           Entry                       as E
import           Graciela
import           SymbolTable
import           MyParseError
import           Parser.Expression
import           Parser.Token
import           Parser.Type
import           Token
import           Type
import           Location
-------------------------------------------------------------------------------
import           Control.Applicative            
import           Control.Lens                   (use, (%=))
import           Control.Monad                  (when,void )
import           Control.Monad.Trans.State.Lazy
import           Data.Functor.Identity
import qualified Data.Text                      as T
import           Text.Megaparsec                hiding (Token)
import           Prelude                        hiding (lookup)
-------------------------------------------------------------------------------
-- | Se encarga del parseo de las variables y su almacenamiento en la tabla de simbolos.
variableDeclaration :: Graciela Instruction
variableDeclaration = do
  from <- getPosition
  match TokVar
  ids <- identifierAndLoc `sepBy` match TokComma
  ast <- try (withAssign ids) <|> withoutAssign ids
  to  <- getPosition
  let loc = Location(from,to)
  case ast of 
    Nothing   -> return $ NoInstruction loc
    Just inst -> return $ Instruction loc inst

  where 
    -- Try to parse if the declared variables are beign assigned
    withAssign ids = do 
      match TokAssign
      exprs <- expression `sepBy` match TokComma
      match TokColon
      t <- type'
      let len = length ids == length exprs
      when (not len) $ void $ genCustomError "La cantidad de variables es distinta a la de expresiones" 
      mapM_ (\( (id,loc), expr@(Expression _ exprType _) ) -> 
                if t == exprType
                  then do 
                    let entry = Entry id loc $ Var t $ Just expr
                    symbolTable %= insertSymbol id entry
                  else genCustomError ("Intentando asignar una expresion de tipo `"
                                        ++ show exprType ++ "` a una variable de tipo `"
                                        ++ show t ++ "`")  
            ) $ zip ids exprs
      let ids' = map (\(id,_) -> id) ids
      if not len
        then return Nothing
        else return $ Just $ Declaration t ids' exprs
      
    withoutAssign ids = do 
      -- If not followed by an Assign token, then just put all the variables in the symbol table 
      match TokColon
      t <- type'
      mapM_ (\(id,loc) -> symbolTable %= insertSymbol id (Entry id loc (Var t Nothing))) ids
      let ids' = map (\(id,_) -> id) ids
      return $ Just $ Declaration t ids' []

constantDeclaration :: Graciela Instruction
constantDeclaration = do
  from <- getPosition
  match TokConst    
  ids <- identifierAndLoc `sepBy` match TokComma
  match TokAssign
  values <- valueOfConstantExpr `sepBy` match TokComma
  match TokColon
  t <- basicType
  to <- getPosition
  let location = Location(from,to)
  if t == GError
    then do 
      genCustomError ("Se intenta declarar constante de tipo `" ++
                       show t ++"`, pero solo pueden ser de tipos basicos.")  
      return $ NoInstruction location
    else do 
      -- Check if the length of both, constants and values, are the same
      let len = length ids == length values
      when (not len) $ void $ genCustomError "La cantidad de constantes es distinta a la de expresiones" 
      -- Check for each value, if has the correct type
      mapM_ (checkType t) (zip ids values)
      if not len 
        then return $ NoInstruction location
        else do 
          -- Get the ids' text and the assigned expressions
          let exprs = map (\(_,x,_) -> x) values
          let ids'  = map (\(id,_) -> id) ids
          return $ Instruction location (Declaration t ids' exprs)
  where 
    checkType t ((id,loc),(valueType,_,value)) = if valueType == t
      then 
        symbolTable %= insertSymbol id (Entry id loc (E.Const t value))
      else 
        genCustomError ("Intentando asignar una expresion de tipo `" ++ 
                         show valueType++"` a una constante de tipo `"++ 
                         show t ++ "`")



{- Try to parse a literal expression or a constant variable
   in the symbol table returning its value. If the expression is not
   constant, None is returned
-}
valueOfConstantExpr :: Graciela (Type,Expression,Value)
valueOfConstantExpr = do 
  expr <- expression
  case exp' expr of 
    BoolLit  b -> return (GBool , expr, E.B b)
    CharLit  c -> return (GChar , expr, E.C c)
    FloatLit f -> return (GFloat, expr, E.F f)
    IntLit   i -> return (GInt  , expr, E.I i)
    Obj (Object _ _ (Variable name)) -> do 
      st <- use symbolTable 
      case lookup name st of 
        Left _  -> return (GError, expr, E.None)
        Right (Entry _ _ (E.Const t value)) -> return (t,expr,value)
        _       -> return (GError, expr, E.None)
    _  -> return (GError, expr, E.None)

-- Find an identifier and returns it's name and the location
identifierAndLoc :: Graciela (T.Text, Location)
identifierAndLoc  = do 
  from <- getPosition
  id <- identifier
  to <- getPosition
  return (id, Location(from,to))


-- | Verifica las variables utilizadas en la lectura
-- decListWithRead :: Graciela Token -> Graciela [Instruction]
-- decListWithRead follow = do 
  
--   return []
