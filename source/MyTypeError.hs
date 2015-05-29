module MyTypeError where

import Data.Text as T
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
                 | UnaryError      { rtype  :: Type
                                   , unOp   :: OpUn
                                   , loc    :: Location
                                   }
                 | StateError      { rtype  :: Type
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
                                   , acType :: Type
                                   , loc    :: Location
                                   }
                deriving (Show)