module Common
  ( Semigroup(..)
  , module Treelike
  , module Location
  , module Pragma
  , internal
  , toList
  , pack, unpack
  , foldM, forM, forM_, unless, void, when, zipWithM, zipWithM_, (>=>)
  , lift
  , Int32
  , trace, traceM, traceShow, traceShowId, traceShowM
  , ($>)
  ) where

import           Control.Monad             (foldM, forM, forM_, unless, void,
                                            when, zipWithM, zipWithM_, (>=>))
import           Control.Monad.Trans.Class (lift)
import           Data.Foldable             (toList)
import           Data.Functor              (($>))
import           Data.Int                  (Int32)
import           Data.Semigroup            (Semigroup (..))
import           Data.Text                 (pack, unpack)
import           Debug.Trace               (trace, traceM, traceShow,
                                            traceShowId, traceShowM)
import           Location
import           Pragma
import           Treelike

internal :: String -> a
internal = error 
         . ("internal error: " <>) 
         . (<> "\n\tPlease open a [New issue] at\n\t\
               \https://github.com/GracielaUSB/graciela/issues\n\t\
               \with the message above and your .gcl file(s)")
