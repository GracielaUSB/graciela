{-|
Module      : ParserError
Description : Recuperacion de errores
Copyright   : GraCieLa

Modulo donde se encuentra todo lo referente al almacenamiento de las variables
en la tabla de simbolos, mientras se esta realizando el parser.
-}
module ParserError where

import Control.Monad.Trans.State.Lazy
import Data.Functor.Identity
import Text.Parsec
import TokenParser
import Token
import State


-- | Se encarga de descarta tokens hasta llegar a algun follow de la regla.
cleanEntry :: ParsecT [Token.TokenPos] () (StateT ParserState Identity) Token 
           -> ParsecT [TokenPos] () (StateT ParserState Identity) (Token, SourcePos)
cleanEntry laset =
  do pos <- getPosition
     e   <- (lookAhead(laset) <|> lookAhead(parseEOF) <|> parseAnyToken)
     panicMode laset
     return ((e, pos))


-- | Se encarga de ignorar tokens hasta encontrar 'until'
panicMode :: ParsecT [TokenPos] () (StateT ParserState Identity) Token 
          -> ParsecT [TokenPos] () (StateT ParserState Identity) [Token]
panicMode until = manyTill parseAnyToken (lookAhead (until <|> parseEOF))
