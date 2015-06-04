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
                 | AssignError     { name   :: Token
                                   , waType :: Type
                                   , prType :: Type
                                   , loc    :: Location
                                   }    
                 | ArrayCallError  { name   :: Token
                                   , prType :: Type
                                   , loc    :: Location
                                   } 
                 | ArrayDimError   { waDim  :: Int
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
                --deriving (Show)


instance Show MyTypeError where
   show (RepSymbolError  sym pLoc loc) = "La variable " ++ show sym ++ " ya esta declarada en la " ++ show pLoc 
   show (ConstIdError    sym      loc) = "Int"
   show (NonDeclError    sym      loc) = "Int"
   show (ArithmeticError lt rt op loc) = "Int"
   show (BooleanError    lt rt op loc) = "Int"
   show (RelationalError lt rt op loc) = "Int"
   show (UnaryError       t    op loc) = "Int"
   show (StateError       t    s  loc) = "Int"
   show (GuardError       t       loc) = "Int"
   show (CondError                loc) = "Int"
   show (IncomDefError            loc) = "Int"
   show (UndecFunError   sym      loc) = "Int"
   show (NumberArgsError sym      loc) = "Int"       
   show (RetFuncError    sym t    loc) = "Int"
   show (FunArgError        wt pt loc) = "Int"
   show (AssignError   name wt pt loc) = "Int"
   show (ArrayDimError      wd pd loc) = "Int"
   show (ArrayCallError   name pt loc) = "Int"
   show (IntOutOfBounds       val loc) = "Int"
   show (RanError              pt loc) = "Int"
   show (UncountError             loc) = "Int"






















