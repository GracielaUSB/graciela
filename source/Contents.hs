module Contents where

import Location
import Type
import AST

data VarBehavour = Constant | Variable
      deriving (Show, Eq)

data Contents = Contents    { varBeh      :: VarBehavour, symbolLoc :: Location, symbolType :: Type, cAST :: Maybe (AST ()) }
              | ArgProcCont { procArgType :: TypeArg    , symbolLoc :: Location, symbolType :: Type                         }
              | FunctionCon { symbolLoc :: Location, symbolType :: Type                                                     }
        deriving (Eq, Show)
