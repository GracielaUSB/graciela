{-# LANGUAGE NamedFieldPuns #-}

module SourcePos
  ( SourcePos (..)
  , showPos
  , showPos'
  , unsafePos
  ) where
--------------------------------------------------------------------------------
import           Text.Megaparsec.Pos (SourcePos (..), unsafePos)
--------------------------------------------------------------------------------

showPos :: SourcePos -> String
showPos SourcePos { sourceLine, sourceColumn } =
  "en la línea " ++ show sourceLine ++ ", columna " ++ show sourceColumn


showPos' :: SourcePos -> String
showPos' SourcePos { sourceLine, sourceColumn } =
  show (sourceLine, sourceColumn)
