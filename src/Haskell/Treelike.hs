{-|
Module      : Language.Graciela.Treelike
Description : Displays arborescent constructs
Copyright   : © 2015-2016 Graciela USB
Maintainer  : moises+graciela@ackerman.space
Stability   : experimental
Portability : POSIX

Provides a typeclass for datatypes which can behave and can be shown
as Trees. The method 'toTree' converts an instance of the class to a
Tree of String nodes.
-}

module Treelike
    ( Tree (..)
    , Treelike (..)
    , drawTree
    , leaf
    ) where
--------------------------------------------------------------------------------
import           Data.Foldable (toList)
import           Data.Tree     (Forest, Tree (..), drawTree)
--------------------------------------------------------------------------------

-- | The class of Tree-like datatypes, i.e., types which can be
-- shown as Trees.
class Treelike a where
  toTree   :: a -> Tree String
  toForest :: (Foldable f, Functor f) => f a -> Forest String
  toForest = toList . fmap toTree

-- | A helper function for constructing nodes without children.
leaf :: String -> Tree String
leaf s = Node s []
