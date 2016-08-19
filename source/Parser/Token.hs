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
  , addToRecSet
  , popRecSet
  ) where
--------------------------------------------------------------------------------
import           Error
import           Graciela
import           Location
import           Token
--------------------------------------------------------------------------------
import           Control.Lens              (use, (%=), (^.))
import           Control.Monad             (mzero, void)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.State (StateT (..), runStateT, runState, evalState, evalStateT)
import           Data.Int                  (Int32)
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.List.NonEmpty        as NE
import           Data.Monoid               ((<>))
import           Data.Sequence             ((|>))
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text, pack)
import           Text.Megaparsec           (ErrorItem (Tokens), ParseError (..),
                                            between, eof, getPosition,
                                            lookAhead, manyTill, token,
                                            withRecovery, (<|>))
import           Text.Megaparsec.Prim      (MonadParsec)
import qualified Text.Megaparsec.Prim      as Prim (Token)
--------------------------------------------------------------------------------
import Debug.Trace

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


anyToken :: (MonadParsec e s m, Prim.Token s ~ TokenPos) => m Token
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


brackets :: (MonadParsec e s m, Prim.Token s ~ TokenPos, MonadGraciela m)
         => m (Maybe a) -> m (Maybe a)
brackets = f . safe
  where
    f =
      between
        (match TokLeftBracket  <* addToRecSet TokRightBracket)
        (match TokRightBracket <* popRecSet)


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

class Monad g => MonadGraciela g where
  putError :: Location -> Error -> g ()
  safe :: g (Maybe a) -> g (Maybe a)
  addToRecSet :: Token -> g ()
  popRecSet :: g ()

instance Monad m => MonadGraciela (GracielaT m) where
  putError = gPutError
  safe = gSafe
  addToRecSet = gAddToRecSet
  popRecSet = gPopRecSet

instance MonadGraciela g => MonadGraciela (StateT s g) where
  putError l e = lift $ putError l e
  safe p = StateT $ \s -> do
    a' <- safe $ evalStateT p s
    return (a', s)
  addToRecSet t = lift $ addToRecSet t
  popRecSet = lift popRecSet

gPutError :: Monad m => Location -> Error -> GracielaT m ()
gPutError (Location (from, to)) e = GracielaT $ do
  let err = ParseError (NE.fromList [from]) Set.empty Set.empty (Set.singleton e)
  errors %= (|> err)

gSafe :: Monad m => GracielaT m (Maybe a) -> GracielaT m (Maybe a)
gSafe = withRecovery r
  where
    r e = do
      pos <- getPosition

      putError
        (Location (pos, undefined))
        (UnexpectedToken (errorUnexpected e))

      ts <- use recSet
      noneOf ts `manyTill` (lookAhead (void $ oneOf ts) <|> eof)

      pure Nothing

gAddToRecSet :: Monad m => Token -> GracielaT m ()
gAddToRecSet t = GracielaT $
  recSet %= (t:)

gPopRecSet :: Monad m => GracielaT m ()
gPopRecSet = GracielaT $
  recSet %= tail
