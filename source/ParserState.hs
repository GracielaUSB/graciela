module ParserState where

import Text.Parsec
import qualified Text.Parsec.Pos as P
import TokenParser
import Token
import Data.Monoid
import Location
import State
import Control.Monad.State as ST
import MyParseError
import Data.Text as T
import SymbolTable (Contents)

addSymbolParser :: T.Text -> Contents -> MyParser ()
addSymbolParser id c = do ST.modify $ addNewSymbol id c
                          return()

newEmptyError  pos          = EmptyError   { loc = Location (P.sourceLine pos) (P.sourceColumn pos) (P.sourceName pos)                                 }
newParseError  msg (e, pos) = MyParseError { loc = Location (P.sourceLine pos) (P.sourceColumn pos) (P.sourceName pos), waitedTok = msg, actualTok = e }

genNewError :: MyParser (Token) -> WaitedToken -> MyParser ()
genNewError laset msg = do  pos <- cleanEntry laset
                            ST.modify $ addParsingError $ newParseError msg pos
                            return ()

genNewEmptyError :: MyParser ()
genNewEmptyError = do  pos <- getPosition
                       ST.modify $ addParsingError $ newEmptyError pos
                       return ()

cleanEntry laset = do pos <- getPosition
                      e   <- (lookAhead(laset) <|> lookAhead(parseEnd) <|> parseAnyToken)
                      panicMode laset
                      return ((e, pos))

panicMode until = manyTill parseAnyToken (lookAhead (until <|> parseEnd))
