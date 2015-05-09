module Expression where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Text.Parsec.Pos as P
import qualified Control.Monad as M
import Location
import TokenParser
import Token
import Lexer
import AST
import Error
import State
import qualified Data.Monoid as DM
import MyParseError

listExp :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST ()])
listExp follow recSet = do  lookAhead follow
                            return $ return $ []
                        <|> ( do e     <- expr (follow <|> parseComma) (recSet <|> parseComma)
                                 lexp  <- listExpAux follow recSet
                                 return(AP.liftA2 (:) e lexp)
                            )
                        <|> (do err <- genNewError (recSet) (TokenRP)
                                return $ Nothing
                            )

listExpAux :: MyParser Token -> MyParser Token -> MyParser (Maybe [AST ()])
listExpAux follow recSet = do lookAhead follow
                              return $ return $ []
                           <|> ( do parseComma
                                    e  <- expr (follow <|> parseComma) (recSet <|> parseComma)
                                    lexp  <- listExpAux follow recSet
                                    return(AP.liftA2 (:) e lexp)
                               )
                           <|> (do err <- genNewError (recSet) (TokenRP)
                                   return $ Nothing
                               )

expr :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
expr follow recSet =  do lookAhead(follow)
                         return $ Nothing
                      
                      <|> exprLevel1 follow recSet

exprLevel1 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
exprLevel1 follow recSet = do e <- exprLevel2 (follow <|> parseTokEqual) (recSet <|> parseTokEqual)
                              do pos <- getPosition
                                 do (lookAhead (follow) >> return e)
                                    <|> do parseTokEqual
                                           e' <- exprLevel1 follow recSet
                                           return(AP.liftA3 (Relational Equal (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do genNewError (recSet) (Operator)
                                           return $ Nothing

exprLevel2 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
exprLevel2 follow recSet = do e <- exprLevel3 (follow <|> parseTokImplies <|> parseTokConse) (recSet <|> parseTokImplies <|> parseTokConse)
                              do pos <- getPosition
                                 do (lookAhead (follow) >> return e)
                                    <|> do parseTokImplies
                                           e' <- exprLevel2 follow recSet
                                           return(AP.liftA3 (Relational Implies (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do parseTokConse
                                           e' <- exprLevel2 follow recSet
                                           return(AP.liftA3 (Relational Conse (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do genNewError (recSet) (Operator)
                                           return $ Nothing

exprLevel3 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
exprLevel3 follow recSet = do e <- exprLevel4(follow <|> parseOr) (recSet <|> parseOr)
                              do pos <- getPosition
                                 do (lookAhead (follow) >> return e)
                                    <|> do parseOr
                                           e' <- exprLevel3 follow recSet
                                           return(AP.liftA3 (Boolean Dis (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do genNewError (recSet) (Operator)
                                           return $ Nothing
 
exprLevel4 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
exprLevel4 follow recSet = do e <- exprLevel5 (follow <|>  parseAnd) (recSet <|> parseAnd)
                              do pos <- getPosition
                                 do (lookAhead (follow) >> return e)
                                    <|> do parseAnd
                                           e' <- exprLevel4 follow recSet
                                           return(AP.liftA3 (Boolean Con (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do genNewError (recSet) (Operator)
                                           return $ Nothing

exprLevel5 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
exprLevel5 follow recSet = do e <- exprLevel6 (follow <|> parseEqual <|> parseNotEqual) (recSet <|> parseEqual <|> parseNotEqual)
                              do pos <- getPosition
                                 do (lookAhead (follow) >> return e)
                                    <|> do parseEqual
                                           e' <- exprLevel5 follow recSet
                                           return(AP.liftA3 (Relational Equ (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do parseNotEqual
                                           e' <- exprLevel5 follow recSet
                                           return(AP.liftA3 (Relational Ine (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do genNewError (recSet) (Operator)
                                           return $ Nothing

followExprLevelRel = parseTokLess <|> parseTokGreater <|> parseTokLEqual <|> parseTokGEqual

exprLevel6 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
exprLevel6 follow recSet = do e <- exprLevel7 (follow <|> followExprLevelRel) (recSet <|> followExprLevelRel)
                              do pos <- getPosition
                                 do (lookAhead (follow) >> return e)
                                    <|> do parseTokLess
                                           e' <- exprLevel5 follow recSet
                                           return(AP.liftA3 (Relational Less (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do parseTokLEqual
                                           e' <- exprLevel5 follow recSet
                                           return(AP.liftA3 (Relational LEqual (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do parseTokGreater
                                           e' <- exprLevel5 follow recSet
                                           return(AP.liftA3 (Relational Greater (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do parseTokGEqual
                                           e' <- exprLevel5 follow recSet
                                           return(AP.liftA3 (Relational GEqual (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' (return Nothing))
                                        
                                    <|> do genNewError (recSet) (Operator)
                                           return $ Nothing

exprLevel7 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
exprLevel7 follow recSet =  do t <- exprLevel8 (follow <|> parsePlus <|> parseMinus) (recSet  <|> parsePlus <|> parseMinus)
                               do pos <- getPosition
                                  do (lookAhead(follow) >> return t)
                                     <|> do parsePlus
                                            e <- exprLevel7 follow recSet
                                            return $ AP.liftA3 (Arithmetic Sum (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) t e (return Nothing)
                                     
                                     <|> do parseMinus
                                            e <- exprLevel7 follow recSet
                                            return $ AP.liftA3 (Arithmetic Sub (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) t e (return Nothing)
                                     
                                     <|> do genNewError (recSet) (Operator)
                                            return $ Nothing

exprLevel8 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
exprLevel8 follow recSet = do p <- exprLevel9 (follow <|> parseSlash <|> parseStar <|> parseTokMod) (recSet <|> parseSlash <|> parseStar <|> parseTokMod)
                              do pos <- getPosition
                                 do (lookAhead(follow) >> return p)
                                    <|> do parseSlash
                                           e <- exprLevel8 follow recSet
                                           return $ AP.liftA3 (Arithmetic Div (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p e (return Nothing)
                                    
                                    <|> do parseStar
                                           e <- exprLevel8 follow recSet
                                           return $ AP.liftA3 (Arithmetic Mul (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p e (return Nothing)
                                    
                                    <|> do parseTokMod
                                           e <- exprLevel8 follow recSet
                                           return $ AP.liftA3 (Arithmetic Mod (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p e (return Nothing)
                                    
                                    <|> do genNewError (recSet) (Operator)
                                           return $ Nothing

exprLevel9 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
exprLevel9 follow recSet = do p <- exprLevel10 (follow <|> parseTokAccent) (recSet <|> parseTokAccent)
                              do pos <- getPosition
                                 do  (lookAhead(follow) >> return p)
                                     <|> do parseTokAccent
                                            e <- exprLevel9 follow recSet
                                            return $ AP.liftA3 (Arithmetic Exp (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p e (return Nothing)
                                     
                                     <|> do genNewError (recSet) (Operator)
                                            return $ Nothing
                             
{-| La función factor follow se encarga de consumir una expresión simple.
    Ésta puede ser un número, letra, cadena de caracteres, llamada a función,
    etc.
 -}

exprLevel10 :: MyParser Token -> MyParser Token -> MyParser (Maybe (AST()) )
exprLevel10 follow recSet = do do pos <- getPosition
                                  do parseLeftParent
                                     e <- expr (parseRightParent) (parseRightParent)
                                     do  try(parseRightParent >>= return . return e)
                                         <|> do genNewError (recSet) (TokenRP)
                                                return $ Nothing
                                  
                                     <|> (do n <- number
                                             return $ return $ Int (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) n Nothing   
                                         )
                                     <|> do idp <- parseID
                                            do      lookAhead follow
                                                    return $ return $ ID (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) idp Nothing
                                                <|> do parseLeftParent
                                                       lexp <- listExp (parseEnd <|> parseRightParent) (recSet <|> parseRightParent)
                                                       do parseRightParent
                                                          return $ (AP.liftA2 (FCallExp (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) idp) lexp (return Nothing))
                                                          <|> do genNewError (recSet) (TokenRP)
                                                                 return $ Nothing
                                                        
                                                <|> do blist <- bracketsList follow recSet
                                                       return $ (AP.liftA2 (ArrCall (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) idp) blist (return Nothing))

                                         
                                     <|> do parseMaxInt
                                            return $ return $ Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) True  True Nothing
                                     
                                     <|> do parseMinInt
                                            return $ return $ Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) True  False Nothing
                                     
                                     <|> do parseMaxDouble
                                            return $ return $ Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) False True Nothing
                                     
                                     <|> do parseMinDouble
                                            return $ return $ Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) False False Nothing
                                     
                                     <|> do e <- parseBool
                                            return $ return $ Bool (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) e Nothing
                                     
                                     <|> do e <- parseChar
                                            return $ return $ Char (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) e Nothing
                                     
                                     <|> do e <- parseString
                                            return $ return $ String (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) e Nothing
                                     
                                     <|> do parseToInt
                                            parseLeftParent
                                            e <- expr parseRightParent parseRightParent
                                            parseRightParent
                                            return(AP.liftA2 (Convertion ToInt (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e (return Nothing))
                                         
                                     <|> do parseToDouble
                                            parseLeftParent
                                            e <- expr parseRightParent parseRightParent
                                            parseRightParent 
                                            return(AP.liftA2  (Convertion ToDouble (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e  (return Nothing))
                                         
                                     <|> do parseToString
                                            parseLeftParent
                                            e <- expr parseRightParent parseRightParent
                                            parseRightParent
                                            return(AP.liftA2  (Convertion ToString (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e (return Nothing))
                                         
                                     <|> do parseToChar
                                            parseLeftParent
                                            e <- expr parseRightParent parseRightParent
                                            parseRightParent
                                            return(AP.liftA2  (Convertion ToChar (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e (return Nothing))
                                         
                                     <|> do parseMinus
                                            e <- expr follow recSet
                                            return(AP.liftA2  (Unary Minus (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e (return Nothing))
                                         
                                     <|> do parseTokAbs
                                            e <- expr follow recSet
                                            return(AP.liftA2  (Unary Abs (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e (return Nothing))
                                         
                                     <|> do parseTokSqrt
                                            e <- expr follow recSet
                                            return(AP.liftA2  (Unary Sqrt (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e (return Nothing))
                                         
                                     <|> do parseTokLength
                                            e <- expr follow recSet
                                            return(AP.liftA2  (Unary Length (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e (return Nothing))
                                         
                                     <|> do parseTokNot
                                            e <- expr follow recSet
                                            return(AP.liftA2  (LogicalNot (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e (return Nothing))
                                         
                                     <|> quantification follow recSet
                                     <|> do genNewError (recSet) (Number)
                                            return $ Nothing

quantification follow recSet = do parseTokLeftPer
                                  op <- parseOpCuant
                                  id <- parseID
                                  parseColon
                                  r <- expr(parseColon) (recSet <|> parseColon)
                                  parseColon
                                  t <- expr(parseTokRightPer) (recSet <|> parseTokRightPer)
                                  parseTokRightPer
                                  return(AP.liftA3 (Quant op id) r t (return Nothing))


parseOpCuant = parseTokExist
               <|> parseTokMod
               <|> parseTokMax
               <|> parseTokMin
               <|> parseTokForall
               <|> parseTokNotExist
               <|> parseTokSigma
               <|> parseTokPi
               <|> parseTokUnion

bracketsList follow recSet = do  lookAhead follow
                                 return $ return []
                                 <|> do parseLeftBracket
                                        e <- expr parseRightBracket parseRightBracket
                                        do parseRightBracket
                                           lexp <- bracketsList follow recSet
                                           return(AP.liftA2 (:) e lexp)
                                           -- FALTA ARREGLAR EL CASO RARO
                                           -- Modemos levantarnos del error con un hazte el loco
                                           
                                           <|> do err <- genNewError (follow <|> parseLeftBracket) (TokenRB)
                                                  return $ Nothing
                                              
                                       
