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
  idList <- identifier' `sepBy` match TokComma
  try (withAssign idList) <|> withoutAssign idList
  to <- getPosition
  return $ NoInstruction (Location(from,to))

  where 
    -- Try to parse if the declared variables are beign assigned
    withAssign idList = do 
      match TokAssign
      exprs <- expression `sepBy` match TokComma
      match TokColon
      t <- type'
      let len = length idList == length exprs
      when (not len) $ void $ genCustomError "La cantidad de variables es distinta a la de expresiones" 
      mapM_ (\( (id,loc), (Expression _ exprType _) ) -> 
                if t == exprType
                  then symbolTable %= insertSymbol id (Entry id loc (Var t E.None))
                  else genCustomError ("Intentando asignar una expresion de tipo `"
                                        ++ show exprType ++ "` a una variable de tipo `"
                                        ++ show t ++ "`")  
            ) $ zip idList exprs
      
    withoutAssign idList = do 
      -- If not followed by an Assign token, then just put all the variables in the symbol table 
      match TokColon
      t <- type'
      mapM_ (\(id,loc) -> do symbolTable %= insertSymbol id (Entry id loc (Var t E.None))) idList

constantDeclaration :: Graciela Instruction
constantDeclaration = do
  from <- getPosition
  match TokConst    
  idList <- identifier' `sepBy` match TokComma
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
      let len = length idList == length values
      when (not len) $ void $ genCustomError "La cantidad de constantes es distinta a la de expresiones" 
      -- Check for each value, if has the correct type
      mapM_ (\((id,loc),(valueType,value)) -> if valueType == t
                then symbolTable %= insertSymbol id (Entry id loc (Var t value))
                else genCustomError ("Intentando asignar una expresion de tipo `" ++ 
                                       show valueType++"` a una constante de tipo `"++ 
                                       show t ++ "`")
            ) $ zip idList values

      return $ NoInstruction location



{- Try to parse a literal expression or a constant variable
   in the symbol table returning its value. If the expression is not
   constant, None is returned
-}
valueOfConstantExpr :: Graciela (Type,Value)
valueOfConstantExpr = do 
  expr <- expression
  case exp' expr of 
    BoolLit  b -> return $ (GBool , E.B b)
    CharLit  c -> return $ (GChar , E.C c)
    FloatLit f -> return $ (GFloat, E.F f)
    IntLit   i -> return $ (GInt  , E.I i)
    Obj (Object _ _ (Variable name)) -> do 
      st <- use symbolTable 
      case lookup name st of 
        Left _  -> return (GError,E.None)
        Right (Entry _ _ (E.Const t value)) -> return (t,value)
        Right _ -> return (GError,E.None)
    _  -> return (GError,E.None)

-- Find an identifier and returns it's name and the location
identifier' :: Graciela (T.Text, Location)
identifier'  = do 
  from <- getPosition
  id <- identifier
  to <- getPosition
  return (id, Location(from,to))


-- | Verifica las variables utilizadas en la lectura
-- decListWithRead :: Graciela Token -> Graciela [Instruction]
-- decListWithRead follow = do 
  
--   return []
