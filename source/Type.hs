module Type where

import AST

data TypeArg = In | Out | InOut
      deriving (Eq)


instance Show TypeArg where
   show In    = " Var Int"
   show Out   = " Var Out"
   show InOut = " Var Int/Out"


data Type = MyInt | MyFloat | MyBool  | MyChar   | MyFunction   [Type] Type | MyProcedure [Type] 
                  | MyError | MyEmpty | MyString | MyArray Type [AST ()]     
  deriving (Eq) 



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



