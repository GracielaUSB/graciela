{-|
Module      : Language.Graciela.Location
Description : Location information
Copyright   : © 2015-2016 Graciela USB
Maintainer  : moises+graciela@ackerman.space
Stability   : experimental
Portability : POSIX

Provides a datatype for storing information about the location or
position of Graciela internal constructs.
-}

{-# LANGUAGE NamedFieldPuns #-}

module Location
  ( Location (..)
  , SourcePos (..)
  , Pos
  , gracielaDef
  , gracielaDef'
  , pos
  , showPos
  , unPos
  ) where
--------------------------------------------------------------------------------
import           Data.Semigroup      (Semigroup (..))
import           Text.Megaparsec.Pos (Pos, SourcePos (..), unPos, unsafePos)
--------------------------------------------------------------------------------

-- | This datatype stores information about the location of various
-- Graciela constructs, such as Tokens, AST nodes and Procedure/Function
-- definitions.
data Location = Location (SourcePos, SourcePos) -- ^ A location within a file.
  deriving (Eq, Ord)

instance Show Location where
  show (Location (p0@(SourcePos fn l0 c0), p1@(SourcePos _ l1 c1)))
    | fn == "/GRACIELA/" = "(in the Graciela Definition)"
    | otherwise = fn <> if l0 == l1
      then "(line " <> show (unPos l0) <> ", (col " <> show (unPos c0) <>
        " -> col " <> show (unPos c1) <> "))"
      else "(" <> showPos p0 <> " -> " <> showPos p1 <> ")"

instance Semigroup Location where
  Location (from0, to0) <> Location (from1, to1)
    = Location (from0 `min'` from1, to1 `max'` to1)
    where
      SourcePos fn l0 c0 `min'` SourcePos _ l1 c1
        | l0 < l1 || l0 == l1 && c0 < c1 = SourcePos fn l0 c0
        | otherwise = SourcePos fn l1 c1
      SourcePos fn l0 c0 `max'` SourcePos _ l1 c1
        | l0 > l1 || l0 == l1 && c0 > c1 = SourcePos fn l0 c0
        | otherwise = SourcePos fn l1 c1

pos :: Location -> SourcePos
pos (Location (p, _)) = p

gracielaDef :: Location
gracielaDef = Location (gracielaDef', gracielaDef')

gracielaDef' :: SourcePos
gracielaDef' = SourcePos "/GRACIELA/" (unsafePos 1) (unsafePos 1)

-- | Shows a 'SourcePos' in a human-readable way.
showPos :: SourcePos -> String
showPos SourcePos { sourceName, sourceLine, sourceColumn }
  | sourceName == "/GRACIELA/" = "(in the Graciela Definition)"
  | otherwise =
    "(line " <> show (unPos sourceLine) <> ", col " <>
    show (unPos sourceColumn) <> ")"
