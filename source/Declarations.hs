module Declarations where

import qualified Control.Applicative as AP 
import qualified Control.Monad       as M
import qualified Data.Text           as T
import Contents                      as CO
import ParserState                 
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
decList follow recSet = 
    do lookAhead follow
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
                        return $ AP.liftA2 (++) (fmap (map (\(name,loc') -> AST.ID loc' name t)) idl) rl
                    <|> do parseAssign
                           lexp <- consListParser parseColon (parseColon <|> recSet)
                           parseColon
                           t <- myType parseSemicolon recSet
                           addManySymParser CO.Variable idl t lexp
                           parseSemicolon
                           rl <- decListAux follow recSet
                           let idlist = fmap (map (\(id, loc) -> ID loc id t)) idl
                           return $ AP.liftA2 (:) (M.liftM4 LAssign idlist lexp (return loc) (return MyEmpty)) rl

                 <|> do parseConst
                        idl <- idList (parseAssign) (recSet <|> parseAssign)
                        parseAssign
                        lexp <- consListParser parseColon (parseColon <|> recSet)
                        parseColon
                        t <- myType parseSemicolon recSet
                        addManySymParser CO.Constant idl t lexp
                        parseSemicolon
                        rl <- decListAux follow recSet
                        let idlist = fmap (map (\(id, loc) -> ID loc id t)) idl
                        return $ AP.liftA2 (:) (M.liftM4 LAssign idlist lexp (return loc) (return MyEmpty)) rl


consListParser :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
consListParser follow recSet = 
    do c <- expr (parseComma <|> follow) (parseComma <|> recSet)
       do lookAhead follow
          return $ fmap (:[]) c
          <|> do parseComma
                 l <- consListParser follow recSet
                 return $ AP.liftA2 (:) c l


idList :: MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Location)])
idList follow recSet = 
    do lookAhead (follow)
       genNewEmptyError
       return $ Nothing 
       <|> do ac <- parseID
              loc <- parseLocation
              rl <- idListAux follow recSet
              return (fmap ((ac, loc) :) rl)


idListAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [(T.Text, Location)])
idListAux follow recSet =
    do lookAhead follow
       return $ return []
       <|> do parseComma
              ac <- parseID
              loc <- parseLocation
              rl <- idListAux (follow) (recSet)
              return (fmap ((ac, loc) :) rl)


parseLocation :: MyParser (Location)
parseLocation =
    do pos <- getPosition
       return $ Location (sourceLine pos) (sourceColumn pos) (sourceName pos)
                  
                  
decListWithRead :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST Type])
decListWithRead follow recSet = 
    do ld <- decList (follow <|> parseRead) (recSet <|> parseRead)
       do loc <- parseLocation
          parseRead
          parseLeftParent
          lid <- idList (parseRightParent) (recSet <|> parseRightParent)
          ts <- verifyReadVars lid
          parseRightParent
          do parseWith
             id <- parseString
             parseSemicolon
             return $ AP.liftA2 (++) ld $ fmap (:[]) $ AP.liftA2 (Read loc (Just id) ts) lid (return MyEmpty)
             <|> do parseSemicolon
                    return $ AP.liftA2 (++) ld $ fmap (:[]) $ AP.liftA2 (Read loc Nothing ts) lid (return MyEmpty)
          <|> (return ld)
