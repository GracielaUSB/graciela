module MyTypeError where

import Data.Text as T
import Location
import Token
import Type
import AST
import qualified Data.Text.Read      as TR


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
                 | GuardError      { rtype  :: Type
                                   , loc    :: Location
                                   }   
                 | CondError       { loc    :: Location
                                   }                                                                                            
                 | IncomDefError   { loc    :: Location
                                   }
                 | UndecFunError   { loc    :: Location
                                   , symbol :: T.Text
                                   }
                 | NumberArgsError { loc    :: Location
                                   , symbol :: T.Text
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
                 | IntOutOfBounds  { val      :: T.Text
                                   , location :: Location
                                   }
                 | UncountError    { location :: Location }
                 | NotOccursVar    { symbol   :: T.Text 
                                   , location :: Location
                                   } 
                deriving (Show)
