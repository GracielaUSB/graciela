module Language.Graciela.AST.Type where

import           Data.Serialize

data ArgMode
instance Eq ArgMode
instance Show ArgMode
instance Serialize ArgMode

data Type
instance Eq Type
instance Show Type
instance Serialize Type