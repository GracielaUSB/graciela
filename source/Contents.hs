module Contents where

import Location
import Type
import AST

data VarBehavour = Constant | Variable
      deriving (Eq)


instance Show VarBehavour where
	show Contents.Constant  = " es una Constante"
	show Variable           = " es una Variable"


data Contents = Contents    { varBeh      :: VarBehavour, symbolLoc :: Location, symbolType :: Type, cAST :: Maybe (AST (Type)) }
              | ArgProcCont { procArgType :: TypeArg    , symbolLoc :: Location, symbolType :: Type                             }
              | FunctionCon { symbolLoc :: Location, symbolType :: Type                                                         }
        deriving (Eq)


instance Show Contents where
   show (Contents var loc t ast) = show var  ++ ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc 
   show (ArgProcCont argT loc t) = show argT ++ ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc 
   show (FunctionCon loc t) 	 =              ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc 
