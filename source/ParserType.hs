module ParserType where

import qualified Control.Applicative as AP
import qualified Data.Text as T
import TokenParser
import Text.Parsec
import ParserState
import Location
import State
import Token
import Type
import AST


myBasicType :: MyParser Token -> MyParser Token -> MyParser Type
myBasicType follow recSet = parseType


myType :: MyParser Token -> MyParser Token -> MyParser Type
myType follow recSet = 
    do myBasicType follow recSet
       <|> do parseTokArray
              parseLeftBracket
              n <- parseConstNumber parseOf (recSet <|> parseOf)
              parseRightBracket
              parseOf
              t <- myType follow recSet
              case n of
                Nothing -> return $ MyEmpty
                Just n' -> return $ MyArray n' t

parseConstNumber :: MyParser Token -> MyParser Token -> MyParser (Maybe (Either T.Text Integer))
parseConstNumber follow recSet = 
    do pos <- getPosition
       do  lookAhead follow
           genNewEmptyError
           return $ Nothing
           <|> do e <- number
                  return $ return $ return e
           <|> do id <- parseID
                  lookUpConstIntParser id (getLocation pos)
                  return $ return $ Left id
