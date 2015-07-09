module ParserType where

import qualified Control.Applicative as AP
import qualified Data.Text.Read      as TR
import TokenParser
import Text.Parsec
import ParserState
import Location
import State
import Token
import Type
import AST

myBasicType :: MyParser Token -> MyParser Token -> MyParser (Maybe Type)
myBasicType follow recSet = do t <- parseType
                               return $ return $ t

myType :: MyParser Token -> MyParser Token -> MyParser (Maybe Type)
myType follow recSet = do myBasicType follow recSet
                          <|> do parseTokArray
                                 parseLeftBracket
                                 n <- parseConstNumber parseOf (recSet <|> parseOf)
                                 parseRightBracket
                                 parseOf
                                 t <- myType follow recSet
                                 return $ fmap (MyArray n) t

countableType :: MyParser Token -> MyParser Token -> MyParser (Maybe Type)
countableType follow recSet = do pos  <- getPosition
                                 t'   <- myBasicType follow recSet
                                 case t' of 
                                 { Nothing -> return $ Nothing
                                 ; Just t  -> case t of 
                                              { MyBool     -> return $ return $ t 
                                              ; MyInt      -> return $ return $ t
                                              ; MyChar     -> return $ return $ t
                                              ; otherwise  -> do addUncountableError (getLocation pos)
                                                                 return Nothing          
                                              }
                                 }

parseConstNumber :: MyParser Token -> MyParser Token -> MyParser (Maybe Integer)
parseConstNumber follow recSet = do pos <- getPosition
                                    do  lookAhead follow
                                        genNewEmptyError
                                        return $ Nothing
                                        <|> do e <- number
                                               return $ return e
                                        <|> do id <- parseID
                                               lookUpConstIntParser id (getLocation pos)
