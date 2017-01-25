{-|
Module      : Language.Graciela.Parser.Prim
Description : Primitives for Graciela parser
Copyright   : © 2015-2016 Graciela USB
Maintainer  : moises+graciela@ackerman.space
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Graciela.Parser.Prim
  where
--------------------------------------------------------------------------------
import           Language.Graciela.Token (TokenPos (..))
--------------------------------------------------------------------------------
import qualified Text.Megaparsec.Prim    as Prim
--------------------------------------------------------------------------------

-- | The Graciela parser works on a list of Tokens with position information
-- as its Stream.
instance Prim.Stream [TokenPos] where
  type Token [TokenPos] = TokenPos
  uncons []     = Nothing
  uncons (t:ts) = Just (t, ts)
  {-# INLINE uncons #-}
  updatePos _ _ _ TokenPos {start, end} = (start, end)
  {-# INLINE updatePos #-}
