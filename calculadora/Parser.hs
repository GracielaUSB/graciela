module Parser where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Token
import Lexer

lookAheadExpr = lookAhead(number <|> parseLeftParent <|> parseEnd)

parseExpr = expr parseEnd

expr :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [ParseError] Integer)
expr laset =  do lookAhead(laset)
                 pos <- getPosition
                 return (Left (return (newErrorMessage (Message ("Error: Esperaba una expresion (posible expresion vacia)")) pos)))

              <|> do t <- term'
                     do (lookAhead(laset) >> return t)
                        <|> (parsePlus  AP.*> expr laset >>= return . (verifyBinError (+) t))
                        <|> (parseMinus AP.*> expr laset >>= return . (verifyBinError (-) t))
                        <|> (do err' <- genNewError (lookAheadExpr) ("+ o -") >>= return . (checkError t)
                                e    <- expr laset
                                return(concatError err' e)
                            )
                     <?> "expresion"

concatError (Left errors) (Left errors') = (Left (errors ++ errors'))
concatError (Left errors) _              = (Left errors)

checkError (Left xs) s = Left (s:xs)
checkError _         s = Left [s] 

term' :: Parsec [TokenPos] () (Either [ParseError] Integer)
term' = do p <- factor
           do (lookAhead(parsePlus <|> parseMinus <|> parseEnd <|> parseRightParent) >> return p)
              <|> (parseSlash AP.*> term' >>= return . (verifyBinError (div) p))
              <|> (parseStar  AP.*> term' >>= return . (verifyBinError (*)   p))
              <|> (genNewError (lookAheadSet) ("* o /") >>= return . (checkError p))
              <?> "termino"

panicMode until = manyTill parseAnyToken until 

factor :: Parsec [TokenPos] () (Either [ParseError] Integer)
factor = do try( do parseLeftParent
                    e <- expr (parseEnd <|> parseRightParent)
                    do  try(parseRightParent >>= return . return e)
                        <|>(genNewError (lookAheadSet) (")") >>= return . (checkError e))
               )

            <|> (number >>= return . Right . num)
            <?> "numeros"

genNewError :: Parsec [TokenPos] () (Token) -> String -> Parsec [TokenPos] () (ParseError)
genNewError laset msg = do  pos <- getPosition
                            e <- (lookAhead(number) <|> lookAhead(parseEnd) <|> parseAnyToken)
                            panicMode laset
                            return (newErrorMessage (Message ("Esperaba un " ++ msg ++ " en vez de " ++ (show e))) pos)

verifyBinError _  (Left xs) (Left ys) = Left (xs ++ ys)
verifyBinError _  (Left xs) _         = Left xs
verifyBinError _  _         (Left ys) = Left ys
verifyBinError op (Right x) (Right y) = Right(op x y) 

lookAheadSet = lookAhead( parseMinus 
                      <|> parseStar 
                      <|> parseSlash
                      <|> parsePlus 
                      <|> parseEnd 
                      <|> parseComma 
                      <|> parseLeftParent
                      <|> parseRightParent)
