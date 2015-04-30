module Declarations where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Text.Parsec.Pos as P
import Token
import Lexer
import AST
import Error
import Expression

myBasicType follow recSet = do t <- parseType
                               return(return ((BasicType) 777 t))

myType follow recSet = do myBasicType follow recSet
                          <|> do parseTokArray
                                 lb <- bracketsList parseOf (recSet <|> parseOf)
                                 parseOf
                                 t <- myBasicType follow recSet
                                 return ((fmap ((ArrType) 777) t) AP.<*> lb)

                              

decList follow recSet = do lookAhead follow
                           return (Right [])
                           <|> decListAux follow recSet
                           
decListAux follow recSet = do lookAhead follow
                              return (Right [])
                              <|> do parseVar
                                     idl <- idList (parseColon <|> parseAssign) (recSet <|> parseColon <|> parseAssign)
                                     do     parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            rl <- decListAux follow recSet
                                            return(AP.liftA2 (:) (AP.liftA2 ((DecVar) 777) idl t) rl)
                                        <|> do parseAssign
                                               lexp <- listExp parseColon (recSet <|> parseColon)
                                               parseColon
                                               t <- myType parseSemicolon recSet
                                               parseSemicolon
                                               rl <- decListAux follow recSet
                                               return(AP.liftA2 (:) (AP.liftA3 ((DecVarAgn) 777) idl lexp t) rl)
                                     <|> do parseConst
                                            idl <- idList (parseAssign) (recSet <|> parseAssign)
                                            parseAssign
                                            lexp <- listExp parseColon (recSet <|> parseColon)
                                            parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            rl <- decListAux follow recSet
                                            return(AP.liftA2 (:) (AP.liftA3 ((DecVarAgn) 777) idl lexp t) rl)
                           
idList :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [Token])
idList follow recSet = do lookAhead (follow)
                          pos <- getPosition
                          return (Left (return (newEmptyError pos)))
                          <|> do ac <- parseID
                                 rl <- idListAux follow recSet
                                 return (fmap (ac:) rl)
                              
idListAux :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [Token])
idListAux follow recSet = do lookAhead follow
                             return (Right ([]))
                             <|> do parseComma
                                    ac <- parseID
                                    rl <- idListAux (follow) (recSet)
                                    return (fmap (ac :) rl)

decListWithRead follow recSet = do ld <- decList (follow <|> parseRead) (recSet <|> parseRead)
                                   do parseRead
                                      parseLeftParent
                                      lid <- idList (parseRightParent) (recSet <|> parseRightParent)
                                      parseRightParent
                                      do parseWith
                                         id <- parseString
                                         parseSemicolon
                                         return ((fmap ((DecProcReadFile) 777 id) ld) AP.<*> lid)
                                         <|> do parseSemicolon
                                                return (fmap ((DecProcReadSIO) 777) ld AP.<*> lid)
                                      <|> return(fmap((DecProc) 777) ld)
