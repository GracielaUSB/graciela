module MyTypeError where

import Location
import Token
import Data.Text as T
import AST

data MyTypeError = RepSymbolError { symbol      :: T.Text
                                  , preLocation :: Location
                                  , actLocation :: Location
                                  }
                 | ArithmeticError { arOp   :: OpNum
                                   , actLoc :: Location 
                                   }                 
                 | IncomDefError  { location :: Location }
               deriving (Show)
