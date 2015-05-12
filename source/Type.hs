module Type where

import AST

data TypeArg = In | Out | InOut
      deriving (Eq)


instance Show TypeArg where
   show In    = "de Entrada"
   show Out   = "de Salida"
   show InOut = "de Entrada y Salida"


data Type = MyInt | MyFloat | MyBool | MyChar | MyString |  Function [Type] Type | Array Type [AST ()] | Procedure [Type]
  deriving (Show, Eq) 

