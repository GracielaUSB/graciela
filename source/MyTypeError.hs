module MyTypeError where

import Location
import Token
import Data.Text as T
import AST
import Type
import Token
data MyTypeError = RepSymbolError { symbol      :: T.Text
                                  , preLocation :: Location
                                  , actLocation :: Location
                                  }
                 | ConstIdError   { symbol   :: T.Text
                                  , location :: Location
                                  }
                 | NonDeclError   { symbol   :: T.Text
                                  , location :: Location
                                  }
                 | ArithmeticError { arOp   :: OpNum
                                   , actLoc :: Location 
                                   }                 
                 | IncomDefError  { location :: Location }
                 | UndecFunError { location :: Location
                                 , symbol   :: T.Text
                                 }
                 | NumberArgsError { location :: Location
                                   , symbol   :: T.Text
                                   }
                 | RetFuncError  { symbol   :: T.Text
                                 , ftype    :: Type
                                 , location :: Location
                                 }
                 | FunArgError   { waitedType :: Type
                                 , actualType :: Type
                                 , location   :: Location
                                 }
               deriving (Show)
