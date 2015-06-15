module Type where

data TypeArg = In | Out | InOut
      deriving (Eq)

instance Show TypeArg where
   show In    = " Var Int"
   show Out   = " Var Out"
   show InOut = " Var Int/Out"


data Type = MyInt | MyFloat | MyBool  | MyChar   | MyFunction   [Type] Type | MyProcedure [Type] 
                  | MyError | MyEmpty | MyString | MyArray {getType :: Type, getTam :: Integer}

isTypeProc :: Type -> Bool
isTypeProc (MyProcedure _) = True
isTypeProc _               = False
 
isTypeFunc :: Type -> Bool
isTypeFunc (MyFunction _ _ ) = True
isTypeFunc _                 = False

instance Eq Type where
   MyInt            ==  MyInt           = True
   MyFloat          ==  MyFloat         = True           
   MyBool           ==  MyBool          = True
   MyChar           ==  MyChar          = True
   MyString         ==  MyString        = True
   MyError          ==  MyError         = True
   MyEmpty          ==  MyEmpty         = True
   (MyProcedure  _) == (MyProcedure  _) = True
   (MyFunction _ t) ==  x               = t == x
   x                == (MyFunction _ t) = x == t
   (MyArray    t d) ==  x               = t == x
   x                == (MyArray    t d) = x == t  
   _                ==  _               = False


instance Show Type where
   show  MyInt             = "Int"
   show  MyFloat           = "Double"
   show  MyBool            = "Boolean"
   show  MyString          = "String"
   show  MyChar            = "Char"
   show  MyEmpty           = "Vacio"
   show  MyError           = "Error"
   show (MyFunction xs t ) = "Func, return type: " ++ show t
   show (MyArray    t  xs) = "Array of "     ++ show t 
   show (MyProcedure   xs) = "Proc"


getDimention :: Type -> Int -> Int
getDimention (MyArray t tam) n = getDimention t (n+1)  
getDimention _               n = n


isCuantificable :: Type -> Bool
isCuantificable x = x == MyInt || x == MyFloat || x == MyBool
