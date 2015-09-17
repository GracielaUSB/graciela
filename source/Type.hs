module Type where

import Data.Text

data TypeArg = In | Out | InOut | Ref
      deriving (Eq)

instance Show TypeArg where
   show In    = " Var Int"
   show Out   = " Var Out"
   show InOut = " Var Int/Out"
   show Ref   = " Var Ref"  


data Type = MyInt | MyFloat | MyBool  | MyChar   | MyFunction {  paramType :: [Type], retuType :: Type } | MyProcedure [Type] 
                  | MyError | MyEmpty | MyString | MyArray { getTam :: Either Text Integer, getType :: Type }



instance Eq Type where
   MyInt            ==  MyInt             = True
   MyFloat          ==  MyFloat           = True           
   MyBool           ==  MyBool            = True
   MyChar           ==  MyChar            = True
   MyString         ==  MyString          = True
   MyError          ==  MyError           = True
   MyEmpty          ==  MyEmpty           = True
   (MyProcedure  _) == (MyProcedure  _)   = True
   (MyFunction _ t) == (MyFunction _ t')  = t == t'
   (MyArray    d t) == (MyArray d' t')    = t == t'
   _                ==  _                 = False


instance Show Type where
   show  MyInt             = "Int"
   show  MyFloat           = "Double"
   show  MyBool            = "Boolean"
   show  MyString          = "String"
   show  MyChar            = "Char"
   show  MyEmpty           = "Vacio"
   show  MyError           = "Error"
   show (MyFunction xs t ) = "Func, return type: " ++ show t
   show (MyArray    t  xs) = "Array of "     ++ show t ++ " de tamaño " ++ show xs
   show (MyProcedure   xs) = "Proc"


isTypeProc :: Type -> Bool
isTypeProc (MyProcedure _) = True
isTypeProc _               = False
 
 
isTypeFunc :: Type -> Bool
isTypeFunc (MyFunction _ _ ) = True
isTypeFunc _                 = False


getDimention :: Type -> Int
getDimention (MyArray tam t)= 1 + getDimention t
getDimention _              = 0


isCuantificable :: Type -> Bool
isCuantificable x = x == MyInt || x == MyChar || x == MyBool

