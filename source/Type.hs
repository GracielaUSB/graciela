module Type where

import AST

data TypeArg = In | Out | InOut
      deriving (Show, Eq)

data Type = MyInt | MyFloat | MyBool | MyChar | MyString | Function [Type] Type | Array Type [AST ()] | Procedure [Type]
  deriving (Show, Eq) 
