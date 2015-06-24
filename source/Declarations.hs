module Declarations where

import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Text.Parsec.Pos     as P
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
import ParserType
import Location
import Token
import Lexer
import State
import Type
import AST


decList :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
decList follow recSet = do lookAhead follow
                           return $ return []
                           <|> do dl <- decListAux follow recSet
                                  return $ dl
                           

decListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
decListAux follow recSet = 
    do loc <- parseLocation
       do lookAhead follow
          return $ return []
          <|> do parseVar
                 idl <- idList (parseColon <|> parseAssign) (recSet <|> parseColon <|> parseAssign)
                 do     parseColon
                        t <- myType parseSemicolon recSet
                        addManyUniSymParser idl t
                        parseSemicolon
                        rl <- decListAux follow recSet
                        return $ idl >>= (const t) >>= (const rl)
                    <|> do parseAssign
                           lexp <- consListParser parseColon
                           parseColon
                           t <- myType parseSemicolon recSet
                           addManyUniSymParser idl t
                           parseSemicolon
                           rl <- decListAux follow recSet
                           return $ AP.liftA2 (:) (M.liftM3 (ConsAssign loc) idl lexp t) rl
                 <|> do parseConst
                        idl <- idList (parseAssign) (recSet <|> parseAssign)
                        parseAssign
                        lexp <- consListParser parseColon
                        parseColon
                        t <- myType parseSemicolon recSet
                        addManyConsParser idl t lexp
                        parseSemicolon
                        rl <- decListAux follow recSet
                        return $ AP.liftA2 (:) (M.liftM3 (ConsAssign loc) idl lexp t) rl

consListParser :: MyParser Token -> MyParser (Maybe [AST Type])
consListParser follow = 
    do c <- constant
       do lookAhead follow
          return $ fmap (:[]) c
          <|> do parseComma
                 l <- consListParser follow
                 return $ AP.liftA2 (:) c l

idList :: MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Location)])
idList follow recSet = do lookAhead (follow)
                          genNewEmptyError
                          return $ Nothing 
                          <|> do ac <- parseID
                                 loc <- parseLocation
                                 rl <- idListAux follow recSet
                                 return (fmap ((ac, loc) :) rl)

idListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Location)])
idListAux follow recSet = do lookAhead follow
                             return $ return []
                             <|> do parseComma
                                    ac <- parseID
                                    loc <- parseLocation
                                    rl <- idListAux (follow) (recSet)
                                    return (fmap ((ac, loc) :) rl)

parseLocation = do pos <- getPosition
                   return $ Location (sourceLine pos) (sourceColumn pos) (sourceName pos)
                  
decListWithRead :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
decListWithRead follow recSet = 
    do ld <- decList (follow <|> parseRead) (recSet <|> parseRead)
       do parseRead
          parseLeftParent
          lid <- idList (parseRightParent) (recSet <|> parseRightParent)
          parseRightParent
          do parseWith
             id <- parseString
             parseSemicolon
             return $ lid >>= (const ld)
             <|> do parseSemicolon
                    return $ lid >>= (const ld)
          <|> (return ld)
