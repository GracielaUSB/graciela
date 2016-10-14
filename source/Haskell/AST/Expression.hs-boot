module AST.Expression where

import Treelike (Treelike)

data Expression
instance Eq Expression
instance Show Expression
instance Treelike Expression
