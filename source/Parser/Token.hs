{-|
Module      : Parser.Token
Description : Todos los lexemas del lenguaje
Copyright   : Graciela

Contiene los analizadores semánticos (parsers) básicos del compilador,
que funcionan como bloques para analizadores semánticos más complejos.
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

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
import           Graciela
import           Token
--------------------------------------------------------------------------------
import           Data.List.NonEmpty   (NonEmpty ((:|)))
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Text.Megaparsec      (ErrorItem (Tokens), token, between)
import           Text.Megaparsec      hiding (Token, satisfy, oneOf)
--------------------------------------------------------------------------------

unex :: TokenPos -> (Set (ErrorItem TokenPos), Set a, Set b)
unex = (, Set.empty, Set.empty) . Set.singleton . Tokens . (:|[])


match :: Token -> Graciela Token
match t = token test Nothing
  where
    test tp @ TokenPos {tok} =
      if t == tok
        then Right t
        else Left . unex $ tp


satisfy :: (Token -> Bool) -> Graciela Token
satisfy f = token test Nothing
  where
    test tp @ TokenPos {tok} =
      if f tok
        then Right tok
        else Left . unex $ tp


oneOf :: [Token] -> Graciela Token
oneOf ts = satisfy (`elem` ts)


parens :: Graciela a -> Graciela a
parens = between (match TokLeftPar) (match TokRightPar)


percents :: Graciela a -> Graciela a
percents = between (match TokLeftPercent) (match TokRightPercent)


brackets :: Graciela a -> Graciela a
brackets = between (match TokLeftBracket) (match TokRightBracket)


beginEnd :: Graciela a -> Graciela a
beginEnd = between (match TokBegin) (match TokEnd)


anyToken :: Graciela Token
anyToken = token test Nothing
  where
    test TokenPos {tok} = Right tok


identifier :: Graciela Text
identifier = token test Nothing
  where
    test    TokenPos {tok = TokId i} = Right i
    test tp@TokenPos {tok}           = Left . unex $ tp


boolLit :: Graciela Bool
boolLit = token test Nothing
  where
    test    TokenPos {tok = TokBool b} = Right b
    test tp@TokenPos {tok}             = Left . unex $ tp


charLit :: Graciela Char
charLit = token test Nothing
  where
    test    TokenPos {tok = TokChar c} = Right c
    test tp@TokenPos {tok}             = Left . unex $ tp


stringLit :: Graciela Text
stringLit = token test Nothing
  where
    test    TokenPos {tok = TokString s} = Right s
    test tp@TokenPos {tok}               = Left . unex $ tp


integerLit :: Graciela Integer
integerLit = token test Nothing
  where
    test    TokenPos {tok = TokInteger i} = Right i
    test tp@TokenPos {tok}                = Left . unex $ tp


floatLit :: Graciela Double
floatLit = token test Nothing
  where
    test    TokenPos {tok = TokFloat f} = Right f
    test tp@TokenPos {tok}              = Left . unex $ tp
