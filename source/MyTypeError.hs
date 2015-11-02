module MyTypeError where

import Data.List
import Contents       as C
import Data.Text      as T hiding (foldl)
import Data.Foldable       hiding (foldl)
import Data.Monoid
import Location
import Type
import AST


data MyTypeError = RepSymbolError  { symbol :: T.Text
                                   , preLoc :: Location
                                   , loc    :: Location
                                   }
                 | ConstIdError    { symbol :: T.Text
                                   , loc    :: Location
                                   }
                 | NonDeclError    { symbol :: T.Text
                                   , loc    :: Location
                                   }
                 | ArithmeticError { ltype  :: Type 
                                   , rtype  :: Type
                                   , arOp   :: OpNum
                                   , loc    :: Location 
                                   }
                 | BooleanError    { ltype  :: Type 
                                   , rtype  :: Type
                                   , boOp   :: OpBool
                                   , loc    :: Location 
                                   }                   
                 | RelationalError { ltype  :: Type 
                                   , rtype  :: Type
                                   , reOp   :: OpRel
                                   , loc    :: Location 
                                   }  
                 | UnaryError      { prType :: Type
                                   , unOp   :: OpUn
                                   , loc    :: Location
                                   }
                 | StateError      { prType :: Type
                                   , state  :: StateCond
                                   , loc    :: Location
                                   }  
                 | GuardError      { prType :: Type
                                   , loc    :: Location
                                   }   
                 | CondError       { loc    :: Location
                                   }                                                                                            
                 | IncomDefError   { cont   :: VarBehavour
                                   , loc    :: Location
                                   }
                 | UndecFunError   { symbol :: T.Text
                                   , isFunc :: Bool
                                   , loc    :: Location
                                   }
                 | NumberArgsError { symbol :: T.Text
                                   , isFunc :: Bool
                                   , wtLeng :: Int
                                   , prLeng :: Int
                                   , loc    :: Location
                                   }
                 | RetFuncError    { symbol :: T.Text
                                   , waType :: Type
                                   , prType :: Type
                                   , loc    :: Location
                                   }
                 | FunArgError     { symbol :: T.Text
                                   , isFunc :: Bool
                                   , waType :: Type
                                   , prType :: Type
                                   , loc    :: Location
                                   }
                 | AssignError     { aeName :: T.Text
                                   , waType :: Type
                                   , prType :: Type
                                   , loc    :: Location
                                   }    
                 | ArrayCallError  { arrName:: T.Text
                                   , prType :: Type
                                   , loc    :: Location
                                   } 
                 | ArrayDimError   { arrName:: T.Text
                                   , waDim  :: Int
                                   , prDim  :: Int
                                   , loc    :: Location 
                                   }                                                    
                 | IntOutOfBounds  { val    :: T.Text
                                   , loc    :: Location
                                   }

                 | RanError        { symbol :: T.Text
                                   , prType :: Type
                                   , loc    :: Location
                                   }
                 | UncountError    { op     :: OpQuant
                                   , loc    :: Location 
                                   }
                 | NotOccursVar    { op     :: OpQuant
                                   , symbol :: T.Text 
                                   , loc    :: Location
                                   } 
                 | FunNameError    { symbol :: T.Text 
                                   , loc    :: Location
                                   } 
                 | InvalidPar      { name   :: T.Text
                                   , tree   :: AST Type
                                   , loc    :: Location
                                   } 
                 | DiffSizeError   { loc    :: Location 
                                   }
                 | TypeDecError    { symbol :: T.Text
                                   , loc    :: Location
                                   , tyExp  :: Type
                                   , tyVar  :: Type
                                   }
                 | QuantRangeError { op     :: OpQuant
                                   , trange :: Type
                                   , loc    :: Location
                                   } 
                 | QuantIntError   { op     :: OpQuant
                                   , tterm  :: Type
                                   , loc    :: Location
                                   }
                 | QuantBoolError  { op     :: OpQuant
                                   , tterm  :: Type
                                   , loc    :: Location
                                   }
                 | NotIntError     { symbol :: T.Text
                                   , loc    :: Location
                                   }
                 | NotConstError   { symbol :: T.Text
                                   , loc    :: Location
                                   }
                 | NotInitError    { symbol :: T.Text
                                   , loc    :: Location
                                   }
                 | NotRValueError  { symbol :: T.Text
                                   , loc    :: Location
                                   }
                 | IntError        { symbol :: T.Text
                                   , loc    :: Location
                                   }
                                   

instance Show MyTypeError where
   show (RepSymbolError     sym pLoc loc) = 
            errorL loc ++ ": La variable "  ++ show sym ++ " ya fue previamente declarada " ++ show pLoc ++ "."
   show (ConstIdError       sym      loc) = 
            errorL loc ++ ": No se puede cambiar el valor de la constante " ++ show sym ++ "."
   show (NonDeclError       sym      loc) = 
            errorL loc ++ ": La variable " ++ show sym ++ " no está declarada."
   show (ArithmeticError    lt rt op loc) = 
            errorL loc ++ ": Tipos incompatibles en el operador aritmético " ++ show op ++ ", se suministró el tipo " ++ show lt ++ " & " ++ show rt ++ "."
   show (BooleanError       lt rt op loc) = 
            errorL loc ++ ": Tipos incompatibles en el operador booleano " ++ show op ++ ", se suministró el tipo " ++ show lt ++ " & " ++ show rt ++ "."
   show (RelationalError    lt rt op loc) = 
            errorL loc ++ ": Tipos incompatibles en el operador relacional " ++ show op ++ ", se suministró el tipo " ++ show lt ++ " & " ++ show rt ++ "."
   show (UnaryError          t Minus loc) = 
            errorL loc ++ ": Tipo incompatible en el operador unario Negativo, se suministró el tipo " ++ show t ++ ", se esperaba el tipo int o double."  
   show (UnaryError          t Not   loc) = 
            errorL loc ++ ": Tipo incompatible en el operador unario Negación, se suministró el tipo " ++ show t ++ ", se esperaba el tipo boolean."  
   show (UnaryError          t    op loc) = 
            errorL loc ++ ": Tipo incompatible en el operador unario " ++ show op ++ ", se suministró el tipo " ++ show t ++ ", se esperaba el tipo int."  
   show (StateError          t Bound loc) = 
            errorL loc ++ ": Se esperaba en la Función de Cota una expresión de tipo int."   
   show (StateError          t s     loc) = 
            errorL loc ++ ": Se esperaba en la " ++ show s ++ " una expresión de tipo boolean."
   show (GuardError          t       loc) = 
            errorL loc ++ ": Se esperaba un tipo boolean en la guardia, se suministró " ++ show t ++ "."
   show (CondError                   loc) = 
            errorL loc ++ ": Los condicionales no son del mismo tipo."
   show (IncomDefError C.Constant    loc) = 
            errorL loc ++ ": Definición de constantes incompleta."
   show (IncomDefError C.Variable    loc) = 
            errorL loc ++ ": Definición de variables incompleta."
   show (UndecFunError   sym  True   loc) = 
            errorL loc ++ ": La función " ++ show sym ++ " no se puede usar, no esta definida."
   show (UndecFunError   sym  False  loc) = 
            errorL loc ++ ": El procedimiento " ++ show sym ++ " no se puede usar, no esta definido."
   show (NumberArgsError sym True wtL prL loc) = 
            errorL loc ++ ": El número de argumentos de la función " ++ show sym ++ " es inválido, se esperaba " ++ show wtL ++ " argumentos, se suministró " ++ show prL ++ "."
   show (NumberArgsError sym False wtL prL loc) = 
            errorL loc ++ ": El número de argumentos del procedimiento " ++ show sym ++ " es inválido, se esperaba " ++ show wtL ++ " argumentos, se suministró " ++ show prL ++ "."
   show (RetFuncError    sym wt  pt  loc) = 
            errorL loc ++ ": En la función " ++ show sym ++ " se esperaba que retornará el tipo " ++ show wt ++ ", se suministró " ++ show pt ++ "."
   show (FunArgError sym True  wt pt loc) = 
            errorL loc ++ ":La función " ++ show sym ++ " esperaba un argumento de tipo " ++ show wt ++ ", se suministró " ++ show pt ++ "."
   show (FunArgError sym False wt pt loc) = 
            errorL loc ++ ":El procedimiento " ++ show sym ++ " esperaba un argumento de tipo " ++ show wt ++ ", se suministró " ++ show pt ++ "."
   show (AssignError       sym wt pt loc) = 
            errorL loc ++ ": La Variable " ++ show sym ++ " es de tipo " ++ show wt ++ ", se suministró " ++ show pt ++ "."
   show (ArrayDimError     sym wd pd loc) = 
            errorL loc ++ ": El Arreglo " ++ show sym ++ " es de dimensión " ++ show wd ++ ", se suministró dimensión " ++ show pd ++ "." 
   show (ArrayCallError    sym    pt loc) =
            errorL loc ++ ": El índice del arreglo " ++ show sym ++ " debe ser de tipo int, se suministró " ++ show pt ++ "."
   show (IntOutOfBounds          val loc) = 
            errorL loc ++ ": int " ++ show val ++ " fuera del rango representable."
   show (RanError          sym    pt loc) = 
            errorL loc ++ ": La variable" ++ show sym ++ " es de tipo " ++ show pt ++ ", se esperaba varible de tipo int o double."
   show (UncountError  op            loc) = 
            errorL loc ++ ": Tipo no contable en la definición del Cuantificador " ++ show op ++ "." 
   show (NotOccursVar  op  sym       loc) = 
            errorL loc ++ ": La varible " ++ show sym ++ " no ocurre dentro del rango del Cuantificador " ++ show op ++ "."
   show (FunNameError  id            loc) = 
            errorL loc ++ ": El parámetro " ++ show id ++ " es del mismo nombre de la función que está siendo definida."
   show (InvalidPar  name _         loc) = 
            errorL loc ++ ": En el procedimiento " ++ show name ++ " se suministrado una constante como parámetro de salida." 
   show (DiffSizeError                loc) = 
            errorL loc ++ ": El número de variables declaradas es distinto al de expresiones de inicialización encontradas."
   show (TypeDecError  id loc te tv) = 
            errorL loc ++ ": La variable " ++ show id ++ " es del tipo " ++ show tv ++ " pero su expresión correspondiente es del tipo " ++ show te ++ "."
   show (QuantRangeError op tr loc) = 
            errorL loc ++ ": El Cuantificador " ++ show op ++ " esperaba un rango del tipo boolean, se suministró " ++ show tr ++ "."
   show (QuantIntError  op tt loc) = 
            errorL loc ++ ": El Cuantificador " ++ show op ++ " esperaba un término del tipo int o double, se suministró " ++ show tt ++ "."
   show (QuantBoolError  op tt loc) = 
            errorL loc ++ ": El Cuantificador " ++ show op ++ " esperaba un término del tipo boolean, se suministró " ++ show tt ++ "."
   show (NotConstError  id            loc) = 
            errorL loc ++ ": La variable " ++ show id ++ " no es constante."
   show (NotIntError  id            loc) = 
            errorL loc ++ ": La variable " ++ show id ++ " no es del tipo int"
   show (NotInitError  id           loc) = 
            errorL loc ++ ": La variable " ++ show id ++ " no está inicializada."
   show (NotRValueError  id           loc) = 
            errorL loc ++ ": La variable " ++ show id ++ " no es un r-value."
   show (IntError  id           loc) = 
            errorL loc ++ ": La variable " ++ show id ++ " no es del tipo int."


checkErrorPosT :: MyTypeError -> MyTypeError -> Ordering
checkErrorPosT x y = getFirstLoc (loc x) (loc y)  

--drawTypeError list = foldl (\acc i -> acc `mappend` show i `mappend` "\n") "\n\n\nERRORES DE TIPOS:\n\n" (toList list)

drawTypeError list = let list'' = sortBy checkErrorPosT list 
                     in foldl (\acc i -> acc `mappend` show i `mappend` "\n") "\n" (toList list'')


