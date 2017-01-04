{-# LANGUAGE LambdaCase #-}

module Shared 
  ( Basic (..)
  , showIn
  ) where

--------------------------------------------------------------------------------
import Data.Int (Int32)
import           Numeric                   (showFFloat)
--------------------------------------------------------------------------------

data Basic
  = BBool  Bool
  | BChar  Char
  | BFloat Double
  | BInt   Int32
  deriving (Eq, Ord)

instance Show Basic where
  show = \case
    BBool  b -> if b then "true" else "false"
    BChar  c -> [c]
    BFloat f -> showFFloat (Just 6) f ""
    BInt   i -> show i

showIn :: Basic -> String
showIn = \case
    BBool  b -> if b then "1" else "0"
    BChar  c -> [c]
    BFloat f -> show f
    BInt   i -> show i
