module Type where

import AST

data TypeArg = In | Out | InOut
      deriving (Eq)


instance Show TypeArg where
   show In    = " Var Int"
   show Out   = " Var Out"
   show InOut = " Var Int/Out"


data Type = MyInt | MyFloat | MyBool | MyChar | MyString |  MyFunction [Type] Type | MyProcedure [Type] 
                  | MyArray Type [AST ()] | MyError String  | MyEmpty
  deriving (Eq) 



instance Show Type where
   show  MyInt             = "Int"
   show  MyFloat           = "Double"
   show  MyBool            = "Boolean"
   show  MyString          = "String"
   show  MyEmpty           = "Vacio"
   show  MyChar            = "Char"
   show (MyFunction xs t ) = "Func, return type: " ++ show t
   show (MyArray    t  xs) = "Array of "     ++ show t 
   show (MyProcedure   xs) = "Proc"
   show (MyError      err) =  err



