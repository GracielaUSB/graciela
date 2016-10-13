module Common
  ( Semigroup(..)
  , module Treelike
  -- , trace, traceM, traceShow, traceShowId, traceShowM
  ) where

import           Data.Semigroup (Semigroup (..))
import           Debug.Trace    (trace, traceM, traceShow, traceShowId,
                                 traceShowM)
import           Treelike
