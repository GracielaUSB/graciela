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

myBasicType :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
myBasicType follow recSet = do t <- parseType
                               return(return (BasicTypeNode t))

myType :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
myType follow recSet = do myBasicType follow recSet
                          <|> do parseTokArray
                                 lb <- bracketsList parseOf (recSet <|> parseOf)
                                 parseOf
                                 t <- myBasicType follow recSet
                                 return ((fmap (ArrTypeNode) t) AP.<*> lb)

                              

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
                                            return(AP.liftA2 (:) (AP.liftA2 (DecVarNode) idl t) rl)
                                        <|> do parseAssign
                                               lexp <- listExp parseColon (recSet <|> parseColon)
                                               parseColon
                                               t <- myType parseSemicolon recSet
                                               parseSemicolon
                                               rl <- decListAux follow recSet
                                               return(AP.liftA2 (:) (AP.liftA3 (DecVarAgnNode) idl lexp t) rl)
                                     <|> do parseConst
                                            idl <- idList (parseAssign) (recSet <|> parseAssign)
                                            parseAssign
                                            lexp <- listExp parseColon (recSet <|> parseColon)
                                            parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            rl <- decListAux follow recSet
                                            return(AP.liftA2 (:) (AP.liftA3 (DecVarAgnNode) idl lexp t) rl)
                           
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
                                         return ((fmap (DecProcReadFileNode id) ld) AP.<*> lid)
                                         <|> do parseSemicolon
                                                return (fmap (DecProcReadSIONode) ld AP.<*> lid)
                                      <|> return(fmap(DecProcNode) ld)

