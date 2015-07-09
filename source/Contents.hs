module Contents where

import Data.Text as T
import Location
import Type


data VarBehavour = Constant | Variable
      deriving (Eq)


instance Show VarBehavour where
	show Contents.Constant  = " es una Constante"
	show Variable           = " es una Variable"


data Value = I Integer | C Char | D Double | S String | B Bool
    deriving (Show, Eq)


data Contents s = Contents    { varBeh      :: VarBehavour, symbolLoc :: Location, symbolType :: Type, value :: Maybe Value }
                | ArgProcCont { procArgType :: TypeArg    , symbolLoc :: Location, symbolType :: Type                       }
                | FunctionCon { symbolLoc :: Location, symbolType :: Type                                                   }
                | ProcCon     { symbolLoc :: Location, symbolType :: Type, nameArgs :: [T.Text], sTable :: s                }
        deriving (Eq)


instance Show a => Show (Contents a) where
   show (Contents var loc t _)    = show var  ++ ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc 
   show (ArgProcCont argT loc t)  = show argT ++ ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc 
   show (FunctionCon loc t)       =              ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc
   show (ProcCon _ _ ln sb)       = show ln ++ show sb


getVarBeh :: Contents a -> Maybe VarBehavour
getVarBeh (Contents vb _ _ _) = Just vb
getVarBeh _                   = Nothing


getProcArgType :: Contents a -> Maybe TypeArg
getProcArgType (ArgProcCont pat _ _) = Just pat
getProcArgType _                     = Nothing
 

