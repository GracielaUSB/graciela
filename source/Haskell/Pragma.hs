module Pragma
  ( Pragma (..)
  ) where
--------------------------------------------------------------------------------

data Pragma
  = LogicAnywhere
  | EnableTrace
  deriving (Eq, Ord, Show)
