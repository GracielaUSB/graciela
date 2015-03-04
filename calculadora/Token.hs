module Token where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T

data Token =   TokPlus
             | TokMinus 
             | TokStar 
             | TokSlash 
             | TokEnd 
             | TokComma 
             | TokLeftParent 
             | TokRightParent 
             | TokInteger {num :: Integer}
             | TokError T.Text
      deriving (Read, Eq)

instance Show Token where
  show TokPlus          = "+"
  show TokMinus         = "-"
  show TokStar          = "*"
  show TokSlash         = "/"
  show TokEnd           = "fin de archivo"
  show TokComma         = ","
  show TokLeftParent    = "paréntesis izquierdo"
  show TokRightParent   = "paréntesis derecho"
  show (TokInteger b)   = "numero : " ++ show b
  show (TokError   e)   = "cadena no reconocida " ++ show e

type TokenPos = (Token, SourcePos)

makeTokenParser x = token showTok posTok testTok
                    where
                      showTok (t, pos) = show t
                      posTok  (t, pos) = pos
                      testTok (t, pos) = if x == t then Just (t) else Nothing

verify :: Token -> Parsec ([TokenPos]) () (Token)
verify token = makeTokenParser token

parsePlus = verify TokPlus
parseMinus = verify TokMinus
parseSlash = verify TokSlash
parseStar = verify TokStar
parseComma = verify TokComma
parseLeftParent = verify TokLeftParent
parseRightParent = verify TokRightParent
parseEnd  = verify TokEnd

parseAnyToken :: Parsec ([TokenPos]) () (Token)
parseAnyToken = token showTok posTok testTok
                where
                  showTok (t, pos) = show t
                  posTok  (t, pos) = pos
                  testTok (t, pos) = Just (t)

number :: Parsec ([TokenPos]) () (Token)
number = token showTok posTok testTok
          where
            showTok (t, pos) = show t
            posTok  (t, pos) = pos
            testTok (t, pos) = case t of
                                { TokInteger n -> Just (TokInteger n)
                                ; otherwise    -> Nothing
                                }
