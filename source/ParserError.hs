module ParserError where

import Text.Parsec
import TokenParser


cleanEntry laset = do pos <- getPosition
                      e   <- (lookAhead(laset) <|> lookAhead(parseEnd) <|> parseAnyToken)
                      panicMode laset
                      return ((e, pos))


panicMode until = manyTill parseAnyToken (lookAhead (until <|> parseEnd))
