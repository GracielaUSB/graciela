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
import Contents                      as CO
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
                                 bl <- bracketsList parseOf (recSet <|> parseOf)
                                 parseOf
                                 t <- myBasicType follow recSet
                                 return (AP.liftA2 (Array) t bl)

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
                                            addManyUniSymParser idl t
                                            rl <- decListAux follow recSet
                                            return((idl >>= (const t)) >>= (const rl) >>= (const (return EmptyAST)))
                                        <|> do parseAssign
                                               lexp <- listExp parseColon (recSet <|> parseColon)
                                               parseColon
                                               t <- myType parseSemicolon recSet
                                               parseSemicolon
                                               addManySymParser CO.Variable idl t lexp
                                               rl <- decListAux follow recSet
                                               return(idl >>= (const t) >>= (const rl) >>= (const lexp) >>= (const (return EmptyAST)))
                                     <|> do parseConst
                                            idl <- idList (parseAssign) (recSet <|> parseAssign)
                                            parseAssign
                                            lexp <- listExp parseColon (recSet <|> parseColon)
                                            parseColon
                                            t <- myType parseSemicolon recSet
                                            parseSemicolon
                                            addManySymParser CO.Constant idl t lexp
                                            rl <- decListAux follow recSet
                                            return(idl >>= (const t) >>= (const rl) >>= (const lexp) >>= (const (return EmptyAST)))

idList :: MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Location)])
idList follow recSet = do lookAhead (follow)
                          genNewEmptyError
                          return $ Nothing 
                          <|> do ac <- parseID
                                 loc <- parseLocation
                                 rl <- idListAux follow recSet
                                 return (fmap ((text ac, loc) :) rl)

idListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Location)])
idListAux follow recSet = do lookAhead follow
                             return $ return []
                             <|> do parseComma
                                    ac <- parseID
                                    loc <- parseLocation
                                    rl <- idListAux (follow) (recSet)
                                    return (fmap ((text ac, loc) :) rl)

parseLocation = do pos <- getPosition
                   return $ Location (sourceLine pos) (sourceColumn pos) (sourceName pos)
                  
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
