{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Graciela.Pragma
  ( Pragma (..)
  ) where

import           Data.Serialize             (Serialize)
import           GHC.Generics               (Generic)
--------------------------------------------------------------------------------

data Pragma
  = LogicAnywhere
  | EnableTrace
  | MemoryOperations
  deriving (Eq, Ord, Show,Generic, Serialize)
