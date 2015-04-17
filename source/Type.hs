module Type where

data Type = MyInt | MyFloat | MyBool | MyChar | MyString | Function Type | Array Type
  deriving (Show, Read, Eq) 
