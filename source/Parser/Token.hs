{-|
Module      : Parser.Token
Description : Todos los lexemas del lenguaje
Copyright   : Graciela

Contiene los analizadores semánticos (parsers) básicos del compilador,
que funcionan como bloques para analizadores semánticos más complejos.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser.Token
  ( anyToken
  , satisfy
  , match
  , oneOf
  , noneOf
  , parens
  , percents
  , brackets
  , beginEnd
  , identifier
  , identifierAndLoc
  , boolLit
  , charLit
  , stringLit
  , integerLit
  , floatLit
  , MonadGraciela
  , safe
  , putError
  ) where
--------------------------------------------------------------------------------
import           Token
import           Location
import           Error
import           Graciela
--------------------------------------------------------------------------------
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text, pack)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import           Text.Megaparsec       (ErrorItem (Tokens), ParseError(..),
                                        token, between, getPosition, manyTill, (<|>), lookAhead, eof, withRecovery)
import           Data.Int              (Int32)
import           Text.Megaparsec.Prim  (MonadParsec)
import qualified Text.Megaparsec.Prim  as Prim (Token)
import Data.Sequence ((|>))
import qualified Data.List.NonEmpty        as NE
import Control.Lens (use, (%=))
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Monoid ((<>))
--------------------------------------------------------------------------------

unex :: TokenPos -> (Set (ErrorItem TokenPos), Set a, Set b)
unex = (, Set.empty, Set.empty) . Set.singleton . Tokens . (:|[])


match :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
      => Token -> m Location
match t = token test Nothing
  where
    test tp @ TokenPos { tok, start, end } =
      if t == tok
        then Right $ Location (start, end)
        else Left . unex $ tp


satisfy :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
        => (Token -> Bool) -> m Token
satisfy f = token test Nothing
  where
    test tp @ TokenPos {tok} =
      if f tok
        then Right tok
        else Left . unex $ tp


anyToken :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m Token
anyToken = satisfy (const True)


oneOf :: (Foldable f, MonadParsec e s m, Prim.Token s ~ TokenPos)
      => f Token -> m Token
oneOf ts = satisfy (`elem` ts)


noneOf :: (Foldable f, MonadParsec e s m, Prim.Token s ~ TokenPos)
      => f Token -> m Token
noneOf ts = satisfy (`notElem` ts)


parens :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
       => m a -> m a
parens = between (match TokLeftPar) (match TokRightPar)


percents :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m a -> m a
percents = between (match TokLeftPercent) (match TokRightPercent)


brackets :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m a -> m a
brackets = between (match TokLeftBracket) (match TokRightBracket)


beginEnd :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m a -> m a
beginEnd = between (match TokBegin) (match TokEnd)


identifier :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
           => m Text
identifier = token test Nothing
  where
    test    TokenPos {tok = TokId i} = Right i
    test tp@TokenPos {tok}           = Left . unex $ tp


-- | Find an identifier and returns it's name and the location
identifierAndLoc :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
                 => m (Text, Location)
identifierAndLoc  = do
  from <- getPosition
  id <- identifier
  to <- getPosition
  pure (id, Location(from,to))


boolLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
        => m Bool
boolLit = token test Nothing
  where
    test    TokenPos {tok = TokBool b} = Right b
    test tp@TokenPos {tok}             = Left . unex $ tp


charLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
        => m Char
charLit = token test Nothing
  where
    test    TokenPos {tok = TokChar c} = Right c
    test tp@TokenPos {tok}             = Left . unex $ tp


stringLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
          => m Text
stringLit = token test Nothing
  where
    test    TokenPos {tok = TokString s} = Right s
    test tp@TokenPos {tok}               = Left . unex $ tp


integerLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
           => m Int32
integerLit = token test Nothing
  where
    test    TokenPos {tok = TokInteger i} = Right i
    test tp@TokenPos {tok}                = Left . unex $ tp


floatLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m Double
floatLit = token test Nothing
  where
    test    TokenPos {tok = TokFloat f} = Right f
    test tp@TokenPos {tok}              = Left . unex $ tp


class MonadGraciela g where
  safe :: g a -> g (Maybe a)
  putError :: Location -> Error -> g ()
  -- addToRecSet :: Token -> m ()

instance MonadGraciela Graciela where
  putError (Location (from, to)) e = Graciela $ do
    let err = ParseError (NE.fromList [from]) Set.empty Set.empty (Set.singleton e)
    errors %= (|> err)

  safe parser = withRecovery r (Just <$> parser)
    where
      r e = do
        pos <- getPosition

        putError
          (Location (pos, undefined))
          (UnknownError $ "Unexpected " <> concatMap show (errorUnexpected e))

        ts <- use recSet
        noneOf ts `manyTill` (lookAhead (void $ oneOf ts) <|> eof)

        pure Nothing

  -- addToRecSet = algo

instance (Monad m, MonadGraciela m, MonadTrans t) => MonadGraciela (t m) where
  safe = safe
  putError l e = putError l e


-- safe :: (MonadTrans t, Monad (t Graciela))
--      => t Graciela a
--      -> t Graciela (Maybe a)
-- safe parser = withRecovery r (Just <$> parser)
--   where
--     r e = do
--       -- pos <- getPosition
--
--       -- putError
--       --   (Location (pos, undefined))
--       --   (UnknownError $ "Unexpected " <> concatMap show (errorUnexpected e))
--
--       -- ts <- use recSet
--       -- noneOf [] `manyTill` (lookAhead (void $ oneOf []) <|> eof)
--
--       pure Nothing
