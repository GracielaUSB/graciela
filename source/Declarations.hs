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
import Location
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

decList :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST ()))
decList follow recSet = do lookAhead follow
                           return $ return EmptyAST
                           <|> decListAux follow recSet
                           

decListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST ()))
decListAux follow recSet = do lookAhead follow
                              return $ return EmptyAST
                              <|> do parseVar
                                     idl <- idList (parseColon <|> parseAssign) (recSet <|> parseColon <|> parseAssign)
                                     do     parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            rl <- decListAux follow recSet
                                            return((idl >>= (const t)) >>= (const rl) >>= (const (return EmptyAST)))
                                        <|> do parseAssign
                                               lexp <- listExp parseColon (recSet <|> parseColon)
                                               parseColon
                                               t <- myType parseSemicolon recSet
                                               parseSemicolon
                                               rl <- decListAux follow recSet
                                               return(idl >>= (const t) >>= (const rl) >>= (const lexp) >>= (const (return EmptyAST)))
                                     <|> do parseConst
                                            idl <- idList (parseAssign) (recSet <|> parseAssign)
                                            parseAssign
                                            lexp <- listExp parseColon (recSet <|> parseColon)
                                            parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            rl <- decListAux follow recSet
                                            return(idl >>= (const t) >>= (const rl) >>= (const lexp) >>= (const (return EmptyAST)))


                           
--idList :: MyParser Token -> MyParser Token -> MyParser (Maybe [(Token, Location)])
idList follow recSet = do lookAhead (follow)
                          genNewEmptyError
                          return $ Nothing 
                          <|> do ac <- parseID
                                 rl <- idListAux follow recSet
                                 -- loc <- getLocation
                                 --return (fmap ((ac, loc) :) rl)
                                 return (fmap (ac:) rl)

--idListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [(Token, Location)])
idListAux follow recSet = do lookAhead follow
                             return $ return []
                             <|> do parseComma
                                    ac <- parseID
                                    rl <- idListAux (follow) (recSet)
                                    -- loc <- getLocation
                                    --return (fmap ((ac, loc) :) rl)
                                    return (fmap (ac:) rl)


-- getLocation = do pos <- getPosition
--                  return $ Location (sourceLine pos) (sourceColumn pos) (sourceName pos)
                  
decListWithRead :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()))
decListWithRead follow recSet = do ld <- decList (follow <|> parseRead) (recSet <|> parseRead)
                                   do parseRead
                                      parseLeftParent
                                      lid <- idList (parseRightParent) (recSet <|> parseRightParent)
                                      parseRightParent
                                      do parseWith
                                         id <- parseString
                                         parseSemicolon
                                         return((ld >>= (const lid)) >>= (const (return EmptyAST)))
                                         <|> do parseSemicolon
                                                return((ld >>= (const lid)) >>= (const (return EmptyAST)))
                                      <|> return(ld >>= (const (return EmptyAST)))


                                      
