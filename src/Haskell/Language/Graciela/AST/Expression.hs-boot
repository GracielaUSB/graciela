module Language.Graciela.AST.Expression where

import Language.Graciela.Treelike (Treelike)

data Expression
instance Eq Expression
instance Show Expression
instance Treelike Expression
