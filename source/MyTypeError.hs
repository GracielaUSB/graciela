module MyTypeError where

import qualified Data.Text.Read as TR
import Contents                 as C
import Data.Text                as T hiding (foldl)
import Data.Foldable                 hiding (foldl)
import Data.Monoid
import Location
import Token
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
                                   , loc    :: Location
                                   }
                 | NumberArgsError { symbol :: T.Text
                                   , wtLeng :: Int
                                   , prLeng :: Int
                                   , loc    :: Location
                                   }
                 | RetFuncError    { symbol :: T.Text
                                   , waType :: Type
                                   , prType :: Type
                                   , loc    :: Location
                                   }
                 | FunArgError     { waType :: Type
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

                 | RanError        { prType :: Type
                                   , loc    :: Location
                                   }
                 | UncountError    { loc    :: Location 
                                   }
                 | NotOccursVar    { symbol :: T.Text 
                                   , loc    :: Location
                                   } 
                 | FunNameError    { symbol :: T.Text 
                                   , loc    :: Location
                                   } 
                 | InvalidPar      { symbol :: T.Text 
                                   , loc    :: Location
                                   } 
                 | DiffSizeError   { location :: Location }
                 | TypeDecError    { symbol   :: T.Text
                                   , location :: Location
                                   , typeExp  :: Type
                                   , typeVar  :: Type
                                   }



instance Show MyTypeError where
   show (RepSymbolError     sym pLoc loc) = 
            errorL loc ++ ": La variable "  ++ show sym ++ " ya fue previamente declarada " ++ show pLoc ++ "."
   show (ConstIdError       sym      loc) = 
            errorL loc ++ ": No se puede redeclarar una constante."
   show (NonDeclError       sym      loc) = 
            errorL loc ++ ": La variable " ++ show sym ++ " no esta declarada."
   show (ArithmeticError    lt rt op loc) = 
            errorL loc ++ ": Tipos incompatibles en el operador aritmético: " ++ show op ++ ", se encontró el tipo: " ++ show lt ++ " & " ++ show rt ++ "."
   show (BooleanError       lt rt op loc) = 
            errorL loc ++ ": Tipos incompatibles en el operador booleano: " ++ show op ++ ", se encontró el tipo: " ++ show lt ++ " & " ++ show rt ++ "."
   show (RelationalError    lt rt op loc) = 
            errorL loc ++ ": Tipos incompatibles en el operador relacional: " ++ show op ++ ", se encontró el tipo: " ++ show lt ++ " & " ++ show rt ++ "."
   show (UnaryError          t Minus loc) = 
            errorL loc ++ ": Tipo incompatible en el operador unario: Negativo, se encontró el tipo: " ++ show t ++ ", se esperaba el tipo: Int o Doble."  
   show (UnaryError          t Not   loc) = 
            errorL loc ++ ": Tipo incompatible en el operador unario: Negación, se encontró el tipo: " ++ show t ++ ", se esperaba el tipo: Boolean."  
   show (UnaryError          t    op loc) = 
            errorL loc ++ ": Tipo incompatible en el operador unario: Negación, se encontró el tipo: " ++ show t ++ ", se esperaba el tipo: Boolean."  
   show (StateError          t Bound loc) = 
            errorL loc ++ ": Se esperaba en la Función de Cota una expresión de tipo: Int."   
   show (StateError          t s     loc) = 
            errorL loc ++ ": Se esperaba en la " ++ show s ++ "una expresión de tipo: Booleano."
   show (GuardError          t       loc) = 
            errorL loc ++ ": Se esperaba en la guardia el tipo: Boolean, se encontró: " ++ show t ++ "."
   show (CondError                   loc) = 
            errorL loc ++ ": Los condicionales no son del mismo tipo."
   show (IncomDefError C.Constant    loc) = 
            errorL loc ++ ": Definición de constantes incompleta"
   show (IncomDefError C.Variable    loc) = 
            errorL loc ++ ": Definición de variables incompleta"
   show (UndecFunError   sym         loc) = 
            errorL loc ++ ": La función " ++ show sym ++ " no se puede usar, no esta definida."
   show (NumberArgsError sym wtL prL loc) = 
            errorL loc ++ ": El número de argumentos es inválido, se esperaba " ++ show wtL ++ " argumentos, se encontró " ++ show prL ++ "."
   show (RetFuncError    sym wt  pt  loc) = 
            errorL loc ++ ": En la función " ++ show sym ++ " se esperaba que retornará el tipo: " ++ show wt ++ ", se encontró " ++ show pt ++ "."
   show (FunArgError           wt pt loc) = 
            errorL loc ++ ": Se esperar argumento de tipo: " ++ show wt ++ ", se encontró: " ++ show pt ++ "."
   show (AssignError      name wt pt loc) = 
            errorL loc ++ ": La Variable " ++ show name ++ " es de tipo: " ++ show wt ++ ", se encontró: " ++ show pt ++ "."
   show (ArrayDimError    name wd pd loc) = 
            errorL loc ++ ": El Arreglo " ++ show name ++ " es de dimensión " ++ show wd ++ ", se encontró dimensión " ++ show pd ++ "." 
   show (ArrayCallError   name    pt loc) =
            errorL loc ++ ": El índice del arreglo " ++ show name ++ " debe ser de tipo: Int, se encontró: " ++ show pt ++ "."
   show (IntOutOfBounds          val loc) = 
            errorL loc ++ ": Int " ++ show val ++ " fuera del rango representable."
   show (RanError                 pt loc) = 
            errorL loc ++ ": La variable es de tipo: " ++ show pt ++ ", se esperaba varible de tipo: Int o Doble."
   show (UncountError                loc) = 
            errorL loc ++ ": Tipo no contable en la definición del cuantificador." 
   show (NotOccursVar    sym        loc) = 
            errorL loc ++ ": La varible " ++ show sym ++ " no ocurre dentro del rango del cuantificador."
   show (FunNameError  id            loc) = 
            errorL loc ++ ": El parámetro " ++ show id ++ " es del mismo nombre de la función que está siendo definida"
   show (InvalidPar  id            loc) = 
            errorL loc ++ ": El parámetro " ++ show id ++ " es constante y está siendo pasado como parámetro de salida"
   show (DiffSizeError                loc) = 
            errorL loc ++ ": El número de variables declaradas es distinto al de expresiones encontradas"
   show (TypeDecError  id loc te tv) = 
            errorL loc ++ ": La variable " ++ show id ++ " es del tipo " ++ show tv ++ " pero su expresión correspondiente es del tipo " ++ show te

drawTypeError list = foldl (\acc i -> acc `mappend` show i `mappend` "\n") "\n\n\nERRORES DE TIPOS:\n\n" (toList list)
