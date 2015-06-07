module MyTypeError where

import qualified Data.Text.Read as TR
import Data.Text                as T
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
                 | GuardError      { prtype :: Type
                                   , loc    :: Location
                                   }   
                 | CondError       { loc    :: Location
                                   }                                                                                            
                 | IncomDefError   { loc    :: Location
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
                                   , ftype  :: Type
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


instance Show MyTypeError where
   show (RepSymbolError     sym pLoc loc) = 
            errorL loc ++ "La variable "  ++ show sym ++ " ya fue previamente declarada " ++ show pLoc ++ "."
   show (ConstIdError       sym      loc) = 
            errorL loc ++ ": -------------------------------------------------."
   show (NonDeclError       sym      loc) = 
            errorL loc ++ ": La variable " ++ show sym ++ " no esta declarada."
   show (ArithmeticError    lt rt op loc) = 
            errorL loc ++ ": Tipos incompatibles en el operador aritmético: " ++ show op ++ ", se encontró el tipo: " ++ show lt ++ " y " ++ show rt ++ "."
   show (BooleanError       lt rt op loc) = 
            errorL loc ++ ": Tipos incompatibles en el operador booleano: " ++ show op ++ ", se encontró el tipo: " ++ show lt ++ " y " ++ show rt ++ "."
   show (RelationalError    lt rt op loc) = 
            errorL loc ++ ": Tipos incompatibles en el operador relacional: " ++ show op ++ ", se encontró el tipo: " ++ show lt ++ " y " ++ show rt ++ "."
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
            errorL loc ++ ": -------------------------------------------------"
   show (IncomDefError               loc) = 
            errorL loc ++ ": -------------------------------------------------"
   show (UndecFunError   sym         loc) = 
            errorL loc ++ ": La función " ++ show sym ++ "no se puede usar, no esta definida."
   show (NumberArgsError sym wtL prL loc) = 
            errorL loc ++ ": El número de argumentos es inválido, se esperaba " ++ show wtL ++ " argumentos, se encontró " ++ show prL ++ "."
   show (RetFuncError    sym t       loc) = "Int"
   show (FunArgError           wt pt loc) = 
            errorL loc ++ ": Se esperar argumento de tipo: " ++ show wt ++ ", se encontró: " ++ show pt ++ "."
   show (AssignError      name wt pt loc) = 
            errorL loc ++ ": La Variable " ++ show name ++ " es de tipo: " ++ show wt ++ ", se encontró: " ++ show pt ++ "."
   show (ArrayDimError    name wd pd loc) = 
            errorL loc ++ ": El Arreglo " ++ show name ++ " es de dimensión " ++ show wd ++ ", se encontró dimensión " ++ show pd ++ "." 
   show (ArrayCallError   name    pt loc) =
            errorL loc ++ ": El índice del arreglo " ++ show name ++ " debe ser de tipo: Int, se encontró: " ++ show pt ++ "."
   show (IntOutOfBounds          val loc) = 
            errorL loc ++ ": -------------------------------------------------"
   show (RanError                 pt loc) = 
            errorL loc ++ ": La variable es de tipo: " ++ show pt ++ ", se esperaba varible de tipo: Int o Doble."
   show (UncountError                loc) = 
            errorL loc ++ ": -------------------------------------------------"
   show (NotOccursVar    sym         loc) = 
            errorL loc ++ ": La varible " ++ show sym ++ " no ocurre dentro del cuantificador."


















