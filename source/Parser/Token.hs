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
  , parens
  , percents
  , brackets
  , beginEnd
  , identifier
  , boolLit
  , charLit
  , stringLit
  , integerLit
  , floatLit
  ) where
--------------------------------------------------------------------------------
import           Token
--------------------------------------------------------------------------------
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Text.Megaparsec      (ErrorItem (Tokens), token, between)
import           Text.Megaparsec.Prim (MonadParsec)
import qualified Text.Megaparsec.Prim as Prim (Token)
--------------------------------------------------------------------------------

unex :: TokenPos -> (Set (ErrorItem TokenPos), Set a, Set b)
unex = (, Set.empty, Set.empty) . Set.singleton . Tokens . (:|[])


match :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
      => Token -> m ()
match t = token test Nothing
  where
    test tp @ TokenPos {tok} =
      if t == tok
        then Right ()
        else Left . unex $ tp


satisfy :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
        => (Token -> Bool) -> m Token
satisfy f = token test Nothing
  where
    test tp @ TokenPos {tok} =
      if f tok
        then Right tok
        else Left . unex $ tp


oneOf :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
      => [Token] -> m Token
oneOf ts = satisfy (`elem` ts)


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


anyToken :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m Token
anyToken = token test Nothing
  where
    test TokenPos {tok} = Right tok


identifier :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
           => m Text
identifier = token test Nothing
  where
    test    TokenPos {tok = TokId i} = Right i
    test tp@TokenPos {tok}           = Left . unex $ tp


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
           => m Integer
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
