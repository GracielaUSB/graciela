module Language.Graciela.AST.Expression where

import           Language.Graciela.Treelike (Treelike)
import           Data.Serialize

data Expression
instance Eq Expression
instance Show Expression
instance Treelike Expression
instance Serialize Expression
