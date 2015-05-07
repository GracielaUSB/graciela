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
                               return((return ((BasicType) t)) AP.<*> (return Nothing))

myType follow recSet = do myBasicType follow recSet
                          <|> do parseTokArray
                                 lb <- bracketsList parseOf (recSet <|> parseOf)
                                 parseOf
                                 t <- myBasicType follow recSet
                                 return (((fmap (ArrType) t) AP.<*> lb) AP.<*> (return Nothing))

                              

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
                                            return(AP.liftA2 (:) ((AP.liftA2 (DecVar) idl t) AP.<*> (return Nothing)) rl)
                                        <|> do parseAssign
                                               lexp <- listExp parseColon (recSet <|> parseColon)
                                               parseColon
                                               t <- myType parseSemicolon recSet
                                               parseSemicolon
                                               rl <- decListAux follow recSet
                                               return(AP.liftA2 (:) ((AP.liftA3 (DecVarAgn) idl lexp t) AP.<*> (return Nothing)) rl)
                                     <|> do parseConst
                                            idl <- idList (parseAssign) (recSet <|> parseAssign)
                                            parseAssign
                                            lexp <- listExp parseColon (recSet <|> parseColon)
                                            parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            rl <- decListAux follow recSet
                                            return(AP.liftA2 (:) ((AP.liftA3 (DecVarAgn) idl lexp t) AP.<*> (return Nothing)) rl)
                                            
                           
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
                                         return (((fmap ((DecProcReadFile) id) ld) AP.<*> lid) AP.<*> (return Nothing))
                                         <|> do parseSemicolon
                                                return ((fmap (DecProcReadSIO) ld AP.<*> lid) AP.<*> (return Nothing))
                                      <|> return((fmap(DecProc) ld) AP.<*> (return Nothing))
