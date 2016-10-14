module Common
  ( Semigroup(..)
  , module Treelike
  , module Location
  , internal
  , toList
  , foldM, forM, forM_, unless, void, when, zipWithM, zipWithM_, (>=>)
  , lift
  -- , trace, traceM, traceShow, traceShowId, traceShowM
  ) where

-- import Debug.Trace    (trace, traceM, traceShow, traceShowId, traceShowM)
import           Control.Monad  (foldM, forM, forM_, unless, void, when, zipWithM, zipWithM_, (>=>))
import           Control.Monad.Trans.Class (lift)
import           Data.Foldable  (toList)
import           Data.Semigroup (Semigroup (..))
import           Location
import           Treelike

internal :: String -> a
internal = error . ("internal error: " <>)
