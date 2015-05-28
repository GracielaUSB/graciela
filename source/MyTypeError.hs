module MyTypeError where

import Location
import Token
import Data.Text as T
import AST

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
               deriving (Show)
