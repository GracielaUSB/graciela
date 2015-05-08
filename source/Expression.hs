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
                        <|> ( do e     <- expr (follow <|> parseComma) (recSet  <|> parseComma)
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

expr follow recSet =  do lookAhead(follow)
                         pos <- getPosition
                         return (Left (return (newEmptyError pos)))
                      
                      <|> exprLevelEqual follow recSet

exprLevelEqual follow recSet = do e <- exprLevelImpl (follow <|> parseTokEqual) (recSet <|> parseTokEqual)
                                  do pos <- getPosition
                                     do (lookAhead (follow) >> return e)
                                        <|> (do parseTokEqual
                                                e' <- exprLevelEqual follow recSet
                                                return(verifyBin3Error (EqualNode (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                            )
                                        <|> (genNewError (recSet) (Operator) >>= return . (checkError e))

exprLevelImpl follow recSet = do e <- exprLevelOr (follow <|> parseTokImplies <|> parseTokConse) (recSet <|> parseTokImplies <|> parseTokConse)
                                 do pos <- getPosition
                                    do (lookAhead (follow) >> return e)
                                       <|> (do parseTokImplies
                                               e' <- exprLevelImpl follow recSet
                                               return(verifyBin3Error (Boolean Implies (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                           )
                                       <|> (do parseTokConse
                                               e' <- exprLevelImpl follow recSet
                                               return(verifyBin3Error (Boolean Conse (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                           )
                                       <|> (genNewError (recSet) (Operator) >>= return . (checkError e))

exprLevelOr follow recSet = do e <- exprLevelAnd (follow <|> parseOr) (recSet <|> parseOr)
                               do pos <- getPosition
                                  do (lookAhead (follow) >> return e)
                                     <|> (do parseOr
                                             e' <- exprLevelOr follow recSet
                                             return(verifyBin3Error (Boolean Dis (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                         )
                                     <|> (genNewError (recSet) (Operator) >>= return . (checkError e))
 
exprLevelAnd follow recSet = do e <- exprPrecLvl'' (follow <|>  parseAnd) (recSet <|> parseAnd)
                                do pos <- getPosition
                                   do (lookAhead (follow) >> return e)
                                      <|> (do parseAnd
                                              e' <- exprLevelAnd follow recSet
                                              return(verifyBin3Error (Boolean Con (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                          )
                                      <|> (genNewError (recSet) (Operator) >>= return . (checkError e))

exprPrecLvl'' follow recSet = do e <- exprLevelRel (follow <|> parseEqual <|> parseNotEqual) (recSet <|> parseEqual <|> parseNotEqual)
                                 do pos <- getPosition
                                    do (lookAhead (follow) >> return e)
                                       <|> (do parseEqual
                                               e' <- exprPrecLvl'' follow recSet
                                               return(verifyBin3Error (Relational Equ (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                           )
                                       <|> (do parseNotEqual
                                               e' <- exprPrecLvl'' follow recSet
                                               return(verifyBin3Error (Relational Ine (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                            )
                                       <|> (genNewError (recSet) (Operator) >>= return . (checkError e))

followExprLevelRel = parseTokLess <|> parseTokGreater <|> parseTokLEqual <|> parseTokGEqual

exprLevelRel follow recSet = do e <- expr' (follow <|> followExprLevelRel) (recSet <|> followExprLevelRel)
                                do pos <- getPosition
                                   do (lookAhead (follow) >> return e)
                                      <|> (do parseTokLess
                                              e' <- exprLevelRel follow recSet
                                              return(verifyBin3Error (Relational Less (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                          )
                                      <|> (do parseTokLEqual
                                              e' <- exprLevelRel follow recSet
                                              return(verifyBin3Error (Relational LEqual (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                          )
                                      <|> (do parseTokGreater
                                              e' <- exprLevelRel follow recSet
                                              return(verifyBin3Error (Relational Greater (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                          )
                                      <|> (do parseTokGEqual
                                              e' <- exprLevelRel follow recSet
                                              return(verifyBin3Error (Relational GEqual (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e e' Nothing)
                                          )
                                      <|> (genNewError (recSet) (Operator) >>= return . (checkError e))

expr' follow recSet =  do t <- term' (follow <|> parsePlus <|> parseMinus) (recSet  <|> parsePlus <|> parseMinus)
                          do pos <- getPosition
                             do (lookAhead(follow) >> return t)
                                <|> (do parsePlus
                                        t' <- expr' follow recSet
                                        return $ verifyBin3Error (Arithmetic Sum (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) t t' Nothing
                                    )
                                <|> (do parseMinus 
                                        t' <- expr' follow recSet  
                                        return $ verifyBin3Error (Arithmetic Sub (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) t t' Nothing
                                    )
                                <|> (genNewError (recSet) (Operator) >>= return . (checkError t))

term' follow recSet = do p <- factor (follow <|> parseSlash <|> parseStar <|> parseTokMod) (recSet <|> parseSlash <|> parseStar <|> parseTokMod)
                         do pos <- getPosition
                            do (lookAhead(follow) >> return p)
                               <|> (do parseSlash 
                                       p' <- term' follow recSet
                                       return $ verifyBin3Error (Arithmetic Div (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p p' Nothing
                                   )
                               <|> (do parseStar
                                       p' <- term' follow recSet
                                       return $ verifyBin3Error (Arithmetic Mul (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p p' Nothing
                                   )
                               <|> (do parseTokMod   
                                       p' <- term' follow recSet
                                       return $ verifyBin3Error (Arithmetic Mod (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p p' Nothing
                                   )
                               <|> (genNewError (recSet) (Operator) >>= return . (checkError p))

factor follow recSet = do p <- factor' (follow <|> parseTokAccent) (recSet <|> parseTokAccent)
                          do pos <- getPosition
                             do  (lookAhead(follow) >> return p)
                                 <|> (do parseTokAccent 
                                         p' <- factor follow recSet
                                         return $ verifyBin3Error (Arithmetic Mod (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) p p' Nothing
                                     )
                                 <|> (genNewError (recSet) (Operator) >>= return . (checkError p))
                             
{-| La función factor follow se encarga de consumir una expresión simple.
    Ésta puede ser un número, letra, cadena de caracteres, llamada a función,
    etc.
 -}

factor' follow recSet = do do pos <- getPosition
                              do parseLeftParent
                                 e <- expr (parseRightParent) (parseRightParent)
                                 do  try(parseRightParent >>= return . return e)
                                     <|> (genNewError (recSet) (TokenRP) >>= return . (checkError e))
                              
                                 <|> (do n <- number
                                         return $ Right $ Int (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) n Nothing
                                      ) 
                                 <|> (do idp <- parseID
                                         do      lookAhead follow AP.*> return(Right(ID (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) idp Nothing))
                                             <|> do parseLeftParent
                                                    lexp <- listExp (parseEnd <|> parseRightParent) (recSet <|> parseRightParent)
                                                    (try (do parseRightParent
                                                             return ((fmap (\f -> f (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) (fmap (FCallExp idp) lexp)) AP.<*> (return Nothing)) 
                                                         )
                                                     <|> (genNewError (recSet) (TokenRP) >>= return . (checkError lexp))
                                                     )
                                             <|> do blist <- bracketsList follow recSet
                                                    return((fmap (ArrCall (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) idp) blist) AP.<*> (return Nothing))
                                     )
                                 <|> (parseMaxInt    AP.*> return(Right(Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) True  True  Nothing)))
                                 <|> (parseMinInt    AP.*> return(Right(Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) True  False Nothing)))
                                 <|> (parseMaxDouble AP.*> return(Right(Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) False True  Nothing)))
                                 <|> (parseMinDouble AP.*> return(Right(Constant (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) False False Nothing)))
                                 <|> (do e <- parseBool
                                         return $ Right $ Bool (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) e Nothing
                                      )
                                 <|> (do e <- parseChar
                                         return $ Right $ Char (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) e Nothing
                                      )
                                 <|> (do e <- parseString
                                         return $ Right $ String (Location (sourceLine pos) (sourceColumn pos) (sourceName pos)) e Nothing
                                      )
                                 <|> (do parseToInt
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return((fmap (Convertion ToInt (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e) AP.<*> (return Nothing))
                                     )
                                 <|> (do parseToDouble
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return((fmap (Convertion ToDouble (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e) AP.<*> (return Nothing))
                                     )
                                 <|> (do parseToString
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return((fmap (Convertion ToString (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e) AP.<*> (return Nothing))
                                     )
                                 <|> (do parseToChar
                                         parseLeftParent
                                         e <- expr parseRightParent parseRightParent
                                         parseRightParent
                                         return((fmap (Convertion ToChar (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e) AP.<*> (return Nothing))
                                     )
                                 <|> (do parseMinus
                                         e <- expr follow recSet
                                         return((fmap (Unary Minus (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e) AP.<*> (return Nothing))
                                     )
                                 <|> ( do parseTokAbs
                                          e <- expr follow recSet
                                          return((fmap (Unary Abs (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e) AP.<*> (return Nothing))
                                     )
                                 <|> ( do parseTokSqrt
                                          e <- expr follow recSet
                                          return((fmap (Unary Sqrt (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e) AP.<*> (return Nothing))
                                     )
                                 <|> ( do parseTokLength
                                          e <- expr follow recSet
                                          return((fmap (Unary Length (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e) AP.<*> (return Nothing))
                                     )
                                 <|> ( do parseTokNot
                                          e <- expr follow recSet
                                          return( (fmap (LogicalNot (Location (sourceLine pos) (sourceColumn pos) (sourceName pos))) e) AP.<*> (return Nothing))
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
                                  return(((fmap ((Quant)  op id) r) AP.<*> t) AP.<*> (return Nothing))

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
