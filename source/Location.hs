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
  , showPos
  , unPos
  ) where
--------------------------------------------------------------------------------
import           Data.Monoid         ((<>))
import           Text.Megaparsec.Pos (SourcePos (..), unPos)
--------------------------------------------------------------------------------

-- | This datatype stores information about the location of various
-- Graciela constructs, such as Tokens, AST nodes and Procedure/Function
-- definitions.
data Location
  = Location (SourcePos, SourcePos) -- ^ A real location within a file.
  | Rearranged                      -- ^ A location which has been lost because
                                    -- of code rearrangement.
  | GracielaDef                     -- ^ The location of internal Graciela
                                    -- definitions.
  deriving (Eq, Ord)

instance Show Location where
  show (Location (p0@(SourcePos _ l0 c0), p1@(SourcePos _ l1 c1))) =
    if l0 == l1
      then "(line " <> show (unPos l0) <> ", (col " <> show (unPos c0) <>
        " -> col " <> show (unPos c1) <> "))"
      else "(" <> showPos p0 <> " -> " <> showPos p1 <> ")"
  show Rearranged = "()"
  show GracielaDef = "In the Graciela Definition"

-- | Shows a 'SourcePos' in a human-readable way.
showPos :: SourcePos -> String
showPos SourcePos { sourceLine, sourceColumn }
  =  "(line " <> show (unPos sourceLine) <> ", col "
  <> show (unPos sourceColumn) <> ")"
