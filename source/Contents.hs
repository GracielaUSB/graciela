module Contents where

import Location
import Type
import Data.Text               as T

data VarBehavour = Constant | Variable
      deriving (Eq)


instance Show VarBehavour where
	show Contents.Constant  = " es una Constante"
	show Variable           = " es una Variable"

data Value = I Integer | C Char | D Double | S String | B Bool
    deriving (Show, Eq)

data Contents s = Contents    { varBeh      :: VarBehavour, symbolLoc :: Location, symbolType :: Type, value :: Maybe Value, ini :: Bool }
                | ArgProcCont { procArgType :: TypeArg    , symbolLoc :: Location, symbolType :: Type                                    }
                | FunctionCon { symbolLoc :: Location, symbolType :: Type                                                                }
                | ProcCon     { symbolLoc :: Location, symbolType :: Type, nameArgs :: [T.Text], sTable :: s                             }
        deriving (Eq)

isInitialized :: Contents a -> Bool
isInitialized (Contents _ _ _ _ True) = True
isInitialized (ArgProcCont _ _ _)     = True
isInitialized _                       = False

getVarBeh :: Contents a -> Maybe VarBehavour
getVarBeh (Contents vb _ _ _ _) = Just vb
getVarBeh _                     = Nothing

getProcArgType :: Contents a -> Maybe TypeArg
getProcArgType (ArgProcCont pat _ _) = Just pat
getProcArgType _                     = Nothing
 
initSymbolContent :: Contents a -> Contents a
initSymbolContent (Contents vb loc t v _) = Contents vb loc t v True

instance Show a => Show (Contents a) where
   show (Contents var loc t v i)  = show var  ++ ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc ++ " " ++ show v ++ " " ++ show i
   show (ArgProcCont argT loc t)  = show argT ++ ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc 
   show (FunctionCon loc t) 	    =              ", Tipo: " ++ show t  ++ ", Declarada en: " ++ show loc
   show (ProcCon _ _ ln sb)       = show ln ++ show sb
