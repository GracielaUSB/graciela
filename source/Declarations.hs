module Declarations where

import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Text.Parsec.Pos     as P
import qualified Data.Text.Read      as TR
import qualified Control.Monad       as M
import qualified Data.Monoid         as DM
import qualified Data.Text           as T
import MyParseError                  as PE
import ParserState                   as PS
import Text.Parsec.Error
import Text.Parsec
import TokenParser
import Expression
import Location
import Token
import Lexer
import State
import Type
import AST



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

                      

decList :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST ()])
decList follow recSet = do lookAhead follow
                           return $ return []
                           <|> decListAux follow recSet
                           


decListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST ()])
decListAux follow recSet = do lookAhead follow
                              return $ return []
                              <|> do parseVar
                                     idl <- idList (parseColon <|> parseAssign) (recSet <|> parseColon <|> parseAssign)
                                     do     parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            rl <- decListAux follow recSet
                                            return(AP.liftA2 (:) (AP.liftA3 DecVar idl t (return Nothing)) rl)
                                        <|> do parseAssign
                                               lexp <- listExp parseColon (recSet <|> parseColon)
                                               parseColon
                                               t <- myType parseSemicolon recSet
                                               parseSemicolon
                                               rl <- decListAux follow recSet
                                               return(AP.liftA2 (:) (M.liftM4 DecVarAgn idl lexp t (return Nothing)) rl)
                                     <|> do parseConst
                                            idl <- idList (parseAssign) (recSet <|> parseAssign)
                                            parseAssign
                                            lexp <- listExp parseColon (recSet <|> parseColon)
                                            parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            rl <- decListAux follow recSet
                                            return(AP.liftA2 (:) (M.liftM4 DecVarAgn idl lexp t (return Nothing)) rl)


                           
idList :: MyParser Token -> MyParser Token -> MyParser (Maybe [Token])
idList follow recSet = do lookAhead (follow)
                          genNewEmptyError
                          return $ Nothing 
                          <|> do ac <- parseID
                                 rl <- idListAux follow recSet
                                 return (fmap (ac:) rl)
                       


idListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [Token])
idListAux follow recSet = do lookAhead follow
                             return $ return []
                             <|> do parseComma
                                    ac <- parseID
                                    rl <- idListAux (follow) (recSet)
                                    return (fmap (ac :) rl)


decListWithRead :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
decListWithRead follow recSet = do ld <- decList (follow <|> parseRead) (recSet <|> parseRead)
                                   do parseRead
                                      parseLeftParent
                                      lid <- idList (parseRightParent) (recSet <|> parseRightParent)
                                      parseRightParent
                                      do parseWith
                                         id <- parseString
                                         parseSemicolon
                                         return (AP.liftA3 (DecProcReadFile id) ld lid  (return Nothing))
                                         <|> do parseSemicolon
                                                return (AP.liftA3 DecProcReadSIO ld  lid  (return Nothing))
                                      <|> return(AP.liftA2 DecProc ld (return Nothing))


                                      