{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser.Prim
    (
    ) where
--------------------------------------------------------------------------------
import Token (TokenPos (..))
--------------------------------------------------------------------------------
import qualified Text.Megaparsec.Prim as Prim
--------------------------------------------------------------------------------

instance Prim.Stream [TokenPos] where
  type Token [TokenPos] = TokenPos
  uncons [] = Nothing
  uncons (t:ts) = Just (t, ts)
  {-# INLINE uncons #-}
  updatePos _ width _ TokenPos {start, end} = (start, end)
  {-# INLINE updatePos #-}
