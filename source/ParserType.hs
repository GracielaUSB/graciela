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


readType :: String -> Maybe (Type)
readType t =      if t == "boolean" then Just $ MyBool
             else if t == "int"     then Just $ MyInt
             else if t == "double"  then Just $ MyFloat
             else if t == "char"    then Just $ MyChar
             else if t == "string"  then Just $ MyString
             else Nothing

myBasicType :: MyParser Token -> MyParser Token -> MyParser (Maybe Type)
myBasicType follow recSet = do t <- parseType
                               return $ readType $ nType t

myType :: MyParser Token -> MyParser Token -> MyParser (Maybe Type)
myType follow recSet = do myBasicType follow recSet
                          <|> do parseTokArray
                                 parseLeftBracket
                                 n <- parseConstNumber parseOf (recSet <|> parseOf)
                                 parseRightBracket
                                 parseOf
                                 t <- myType follow recSet
                                 return (AP.liftA2 (MyArray) t n)

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

parseConstNumber :: MyParser Token -> MyParser Token -> MyParser (Maybe Int)
parseConstNumber follow recSet = do  lookAhead follow
                                     genNewEmptyError
                                     return $ Nothing
                                     <|> do e <- number
                                            tokenToInt e

tokenToInt :: Token -> MyParser (Maybe Int) 
tokenToInt (TokInteger n) = do pos <- getPosition
                               case TR.decimal n of
                               { Left _         -> do addOutOfBoundsError n (getLocation pos)
                                                      return $ Nothing
                               ; Right (n', _)  -> return $ return n'
                               }
