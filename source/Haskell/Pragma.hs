module Pragma
  ( Pragma (..)
  ) where
--------------------------------------------------------------------------------

data Pragma
  = LogicAnywhere
  | EnableTrace
  | GetAddressOf
  deriving (Eq, Ord, Show)
