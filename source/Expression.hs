module Expression where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Text.Parsec.Pos as P
import Location
import Token
import Lexer
import AST
import Error

listExp follow recSet = do  lookAhead follow
                            return (Right [])
                        <|> ( do e     <- expr (follow <|> parseComma) (recSet <|> parseComma)
                                 lexp  <- listExpAux follow recSet
                                 return(verifyBinError (:) e lexp)
                            )
                        <|> (do err <- genNewError (recSet) (TokenRP)
                                return(Left(return err))
                            )

listExpAux follow recSet = do lookAhead follow
                              return(Right [])
                           <|> ( do parseComma
                                    e  <- expr (follow <|> parseComma) (recSet <|> parseComma)
                                    lexp  <- listExpAux follow recSet
                                    return(verifyBinError (:) e lexp)
                               )
                           <|> (do err <- genNewError (recSet) (TokenRP)
                                   return(Left(return err))
                               )

expr :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
expr follow recSet =  do lookAhead(follow)
                         pos <- getPosition
                         return (Left (return (newEmptyError pos)))
                      
                      <|> exprLevelImpl follow recSet

exprLevelImpl follow recSet = do e <- exprLevelOr (follow <|> parseTokImplies <|> parseTokConse) (recSet <|> parseTokImplies <|> parseTokConse)
                                 do pos <- getPosition
                                    do (lookAhead (follow) >> return e)
                                       <|> (do parseTokImplies
                                               e' <- exprLevelImpl follow recSet
                                               return(verifyBinError (Relational Implies (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e')
                                           )
                                       <|> (do parseTokConse
                                               e' <- exprLevelImpl follow recSet
                                               return(verifyBinError (Relational Conse (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e')
                                           )
                                       <|> (genNewError (recSet) (Operator) >>= return . (checkError e))

exprLevelOr follow recSet = do e <- exprLevelAnd (follow <|> parseOr) (recSet <|> parseOr)
                               do pos <- getPosition
                                  do (lookAhead (follow) >> return e)
                                     <|> (do parseOr
                                             e' <- exprLevelOr follow recSet
                                             return(verifyBinError (Boolean Dis (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e')
                                         )
                                     <|> (genNewError (recSet) (Operator) >>= return . (checkError e))
 
exprLevelAnd follow recSet = do e <- exprPrecLvl'' (follow <|>  parseAnd) (recSet <|> parseAnd)
                                do pos <- getPosition
                                   do (lookAhead (follow) >> return e)
                                      <|> (do parseAnd
                                              e' <- exprLevelAnd follow recSet
                                              return(verifyBinError (Boolean Con (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e')
                                          )
                                      <|> (genNewError (recSet) (Operator) >>= return . (checkError e))

exprPrecLvl'' follow recSet = do e <- exprLevelRel (follow <|> parseEqual <|> parseNotEqual) (recSet <|> parseEqual <|> parseNotEqual)
                                 do pos <- getPosition
                                    do (lookAhead (follow) >> return e)
                                       <|> (do parseEqual
                                               e' <- exprPrecLvl'' follow recSet
                                               return(verifyBinError (Relational Equ (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e')
                                           )
                                       <|> (do parseNotEqual
                                               e' <- exprPrecLvl'' follow recSet
                                               return(verifyBinError (Relational Ine (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e')
                                            )
                                       <|> (genNewError (recSet) (Operator) >>= return . (checkError e))

followExprLevelRel = parseTokLess <|> parseTokGreater <|> parseTokLEqual <|> parseTokGEqual

exprLevelRel follow recSet = do e <- expr' (follow <|> followExprLevelRel) (recSet <|> followExprLevelRel)
                                do pos <- getPosition
                                   do (lookAhead (follow) >> return e)
                                      <|> (do parseTokLess
                                              e' <- exprLevelRel follow recSet
                                              return(verifyBinError (Relational Less (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e')
                                          )
                                      <|> (do parseTokLEqual
                                              e' <- exprLevelRel follow recSet
                                              return(verifyBinError (Relational LEqual (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e')
                                          )
                                      <|> (do parseTokGreater
                                              e' <- exprLevelRel follow recSet
                                              return(verifyBinError (Relational Greater (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e')
                                          )
                                      <|> (do parseTokGEqual
                                              e' <- exprLevelRel follow recSet
                                              return(verifyBinError (Relational GEqual (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e')
                                          )
                                      <|> (genNewError (recSet) (Operator) >>= return . (checkError e))

expr' :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
expr' follow recSet =  do t <- term' (follow <|> parsePlus <|> parseMinus) (recSet  <|> parsePlus <|> parseMinus)
                          do pos <- getPosition
                             do (lookAhead(follow) >> return t)
                                <|> (parsePlus  AP.*> expr' follow recSet >>=  return . (verifyBinError (Arithmetic Sum (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) t))
                                <|> (parseMinus AP.*> expr' follow recSet >>=  return . (verifyBinError (Arithmetic Sub (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) t))
                                <|> (genNewError (recSet) (Operator) >>= return . (checkError t))

term' :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
term' follow recSet = do p <- factor (follow <|> parseSlash <|> parseStar) (recSet <|> parseSlash <|> parseStar)
                         do pos <- getPosition
                            do (lookAhead(follow) >> return p)
                               <|> (parseSlash AP.*> term' follow recSet >>= return . (verifyBinError (Arithmetic Div (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p))
                               <|> (parseStar  AP.*> term' follow recSet >>= return . (verifyBinError (Arithmetic Mul (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p))
                               <|> (genNewError (recSet) (Operator) >>= return . (checkError p))

factor follow recSet = do p <- factor' (follow <|> parseTokAccent) (recSet <|> parseTokAccent)
                          do pos <- getPosition
                             do  (lookAhead(follow) >> return p)
                                 <|> (parseTokAccent AP.*> factor follow recSet >>= return . (verifyBinError (Arithmetic Exp (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p))
                                 <|> (genNewError (recSet) (Operator) >>= return . (checkError p))
                             
{-| La función factor follow se encarga de consumir una expresión simple.
    Ésta puede ser un número, letra, cadena de caracteres, llamada a función,
    etc.
 -}

factor' :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] AST)
factor' follow recSet = do do pos <- getPosition
                              do parseLeftParent
                                 e <- expr (parseRightParent) (parseRightParent)
                                 do  try(parseRightParent >>= return . return e)
                                     <|> (genNewError (recSet) (TokenRP) >>= return . (checkError e))
                              
                                 <|> (number >>= return . Right . Int (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) 
                                 <|> (do idp <- parseID
                                         do      lookAhead follow AP.*> return(Right(ID (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) idp))
                                             <|> do parseLeftParent
                                                    lexp <- listExp (parseEnd <|> parseRightParent) (recSet <|> parseRightParent)
                                                    (try (do parseRightParent
                                                             return (fmap (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) (fmap (FCallExp idp) lexp))
                                                         )
                                                     <|> (genNewError (recSet) (TokenRP) >>= return . (checkError lexp))
                                                     )
                                             <|> do blist <- bracketsList follow recSet
                                                    return(fmap (ArrCall (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) idp) blist)
                                     )
                                 <|> (parseMaxInt    AP.*> return(Right(Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) True  True )))
                                 <|> (parseMinInt    AP.*> return(Right(Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) True  False)))
                                 <|> (parseMaxDouble AP.*> return(Right(Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) False True )))
                                 <|> (parseMinDouble AP.*> return(Right(Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) False False)))
                                 <|> (parseBool      >>= return . Right . Bool (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)))
                                 <|> (parseChar      >>= return . Right . Char (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)))
                                 <|> (parseString    >>= return . Right . String (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)))
                                 <|> (do parseToInt
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return(fmap (Convertion ToInt (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e)
                                     )
                                 <|> (do parseToDouble
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return(fmap (Convertion ToDouble (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e)
                                     )
                                 <|> (do parseToString
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return(fmap (Convertion ToString (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e)
                                     )
                                 <|> (do parseToChar
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return(fmap (Convertion ToChar (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e)
                                     )
                                 <|> (do parseMinus
                                         e <- expr follow recSet
                                         return(fmap (Unary Minus (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e)
                                     )
                                 <|> ( do parseTokAbs
                                          e <- expr follow recSet
                                          return(fmap (Unary Abs (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e)
                                     )
                                 <|> ( do parseTokSqrt
                                          e <- expr follow recSet
                                          return(fmap (Unary Sqrt (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e)
                                     )
                                 <|> ( do parseTokLength
                                          e <- expr follow recSet
                                          return(fmap (Unary Length (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e)
                                     )
                                 <|> ( do parseTokNot
                                          e <- expr follow recSet
                                          return(fmap (LogicalNot (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e)
                                     )
                                 <|> quantification follow recSet
                                 <|> (genNewError recSet Number >>= return . Left . return)

quantification follow recSet = do parseTokLeftPer
                                  op <- parseOpCuant
                                  id <- parseID
                                  parseColon
                                  r <- expr(parseColon) (recSet <|> parseColon)
                                  parseColon
                                  t <- expr(parseTokRightPer) (recSet <|> parseTokRightPer)
                                  parseTokRightPer
                                  return((fmap (Quant op id) r) AP.<*> t) 

parseOpCuant = parseTokExist

bracketsList :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] [AST])
bracketsList follow recSet = do  lookAhead follow
                                 return(Right[])
                             <|> do parseLeftBracket
                                    e <- expr parseRightBracket parseRightBracket
                                    do try( do parseRightBracket
                                               lexp <- bracketsList follow recSet
                                               return(verifyBinError (:) e lexp)
                                               -- FALTA ARREGLAR EL CASO RARO
                                               -- Modemos levantarnos del error con un hazte el loco
                                           )
                                       <|> ( do err <- genNewError (follow <|> parseLeftBracket) (TokenRB)
                                                return(checkError e err)
                                           )
