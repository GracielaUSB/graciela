module Declarations where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Text.Parsec.Pos as P
import qualified Control.Monad as M
import Token
import TokenParser
import Lexer
import AST
import ParserState
import Expression
import Type
import State

myBasicType :: MyParser Token -> MyParser Token -> MyParser (Maybe Type)
myBasicType follow recSet = do t <- parseType
                               return(return (nType t))

myType :: MyParser Token -> MyParser Token -> MyParser (Maybe Type)
myType follow recSet = do myBasicType follow recSet
                          <|> do parseTokArray
                                 bracketsList parseOf (recSet <|> parseOf)
                                 parseOf
                                 t <- myBasicType follow recSet
                                 return (fmap (Array) t)

                              

--decList :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
decList follow recSet = do lookAhead follow
                           return $ return []
                           <|> decListAux follow recSet
                           
--decListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST])
decListAux follow recSet = do lookAhead follow
                              return $ return []
                              <|> do parseVar
                                     idl <- idList (parseColon <|> parseAssign) (recSet <|> parseColon <|> parseAssign)
                                     do     parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            rl <- decListAux follow recSet
                                            return(AP.liftA2 (:) (AP.liftA3 DecVar idl t Nothing) rl)
                                        <|> do parseAssign
                                               lexp <- listExp parseColon (recSet <|> parseColon)
                                               parseColon
                                               t <- myType parseSemicolon recSet
                                               parseSemicolon
                                               rl <- decListAux follow recSet
                                               return(AP.liftA2 (:) (M.liftM4 DecVarAgn idl lexp t Nothing) rl)
                                     <|> do parseConst
                                            idl <- idList (parseAssign) (recSet <|> parseAssign)
                                            parseAssign
                                            lexp <- listExp parseColon (recSet <|> parseColon)
                                            parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            rl <- decListAux follow recSet
                                            return(AP.liftA2 (:) (M.liftM4 DecVarAgn idl lexp t Nothing) rl)
                           
--idList :: MyParser Token -> MyParser Token -> MyParser (Maybe [Token])
idList follow recSet = do lookAhead (follow)
                          genNewEmptyError
                          return $ Nothing 
                          <|> do ac <- parseID
                                 rl <- idListAux follow recSet
                                 return (fmap (ac:) rl)
                              
--idListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [Token])
idListAux follow recSet = do lookAhead follow
                             return $ return []
                             <|> do parseComma
                                    ac <- parseID
                                    rl <- idListAux (follow) (recSet)
                                    return (fmap (ac :) rl)

--decListWithRead :: MyParser Token -> MyParser Token -> MyParser (Maybe AST)
decListWithRead follow recSet = do ld <- decList (follow <|> parseRead) (recSet <|> parseRead)
                                   do parseRead
                                      parseLeftParent
                                      lid <- idList (parseRightParent) (recSet <|> parseRightParent)
                                      parseRightParent
                                      do parseWith
                                         id <- parseString
                                         parseSemicolon
                                         return (AP.liftA3 (DecProcReadFile id) ld lid  Nothing)
                                         <|> do parseSemicolon
                                                return (AP.liftA3 DecProcReadSIO ld  lid  Nothing)
                                      <|> return(AP.liftA2 DecProc ld Nothing)
