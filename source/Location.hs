{-# LANGUAGE NamedFieldPuns #-}

module Location
  ( Location (..)
  , SourcePos (..)
  , showPos
  , showPos'
  , unsafePos
  ) where
--------------------------------------------------------------------------------
import           Data.Monoid         ((<>))
import           Text.Megaparsec.Pos (SourcePos (..), unPos, unsafePos)
--------------------------------------------------------------------------------

showPos :: SourcePos -> String
showPos SourcePos { sourceLine, sourceColumn } =
  "en la línea " <> show sourceLine <> ", columna " <> show sourceColumn


showPos' :: SourcePos -> String
showPos' SourcePos { sourceLine, sourceColumn }
  =  "(line " <> show (unPos sourceLine) <> ", col "
  <> show (unPos sourceColumn) <> ")"


data Location
  = Location (SourcePos, SourcePos)
  | Rearranged
  deriving (Eq, Ord)

instance Show Location where
  show (Location (p0@(SourcePos _ l0 c0), p1@(SourcePos _ l1 c1))) =
    if l0 == l1
      then "(line " <> show (unPos l0) <> ", (col " <> show (unPos c0) <>
        " -> col " <> show (unPos c1) <> "))"
      else "(" <> showPos' p0 <> " -> " <> showPos' p1 <> ")"
  show Rearranged = "()"
  