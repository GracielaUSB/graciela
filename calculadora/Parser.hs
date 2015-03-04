module Parser where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Text.Parsec.Pos as P
import Token
import Lexer

data MyParseError = MyParseError { line      :: P.Line
                                 , column    :: P.Column
                                 , waitedTok :: [Token]
                                 , actualTok :: Token
                                 }
                  | EmptyError   { line    :: P.Line
                                 , column  :: P.Column
                                 }
                  | NumberError  { line      :: P.Line
                                 , column    :: P.Column
                                 , actualTok :: Token
                                 }
               deriving (Show, Read)

newEmptyError  pos       = EmptyError   { line = P.sourceLine pos, column = P.sourceColumn pos }             
newParseError  pos msg e = MyParseError { line = P.sourceLine pos, column = P.sourceColumn pos, waitedTok = msg, actualTok = e }
newNumberError (e, pos)  = NumberError  { line = P.sourceLine pos, column = P.sourceColumn pos, actualTok = e }

genNewError :: Parsec [TokenPos] () (Token) -> [Token] -> Parsec [TokenPos] () (MyParseError)
genNewError laset msg = do  pos <- getPosition
                            e <- (lookAhead(parseEnd) <|> parseAnyToken)
                            panicMode laset
                            return (newParseError pos msg e)

cleanEntry :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (TokenPos)
cleanEntry laset = do pos <- getPosition
                      e   <- (lookAhead(parseEnd) <|> parseAnyToken)
                      panicMode laset
                      return ((e, pos))

parseExpr = expr parseEnd

parseListExpr :: Parsec [TokenPos] () (Either [MyParseError] [Integer])
parseListExpr = do e <- expr (parseEnd <|> parseComma)
                   do parseComma >> parseListExpr >>= return . (verifyBinError (:) e)
                      <|> (parseEnd >> return (fmap (return) e))
                    
expr :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] Integer)
expr laset =  do lookAhead(laset)
                 pos <- getPosition
                 return (Left (return (newEmptyError pos)))

              <|> do t <- term' (laset <|> parsePlus <|> parseMinus) 
                     do (lookAhead(laset) >> return t)
                        <|> (parsePlus  AP.*> expr laset >>= return . (verifyBinError (+) t))
                        <|> (parseMinus AP.*> expr laset >>= return . (verifyBinError (-) t))
                        <|> (genNewError (laset) ([TokPlus, TokMinus]) >>= return . (checkError t))
                            
                     <?> "expresion"

concatError (Left errors) (Left errors') = (Left (errors ++ errors'))
concatError (Left errors) _              = (Left errors)

checkError (Left xs) s = Left (s:xs)
checkError _         s = Left [s] 

term' :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] Integer)
term' laset = do p <- factor (laset <|> parseSlash <|> parseStar)
                 do (lookAhead(laset) >> return p)
                    <|> (parseSlash AP.*> term' laset  >>= return . (verifyBinError (div) p))
                    <|> (parseStar  AP.*> term' laset  >>= return . (verifyBinError (*)   p))
                    <|> (genNewError (laset) ([TokSlash, TokStar]) >>= return . (checkError p))
                    <?> "termino"

panicMode until = manyTill parseAnyToken (lookAhead until)

factor :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] Integer)
factor laset = do do parseLeftParent
                     e <- expr (parseEnd <|> parseComma <|> parseRightParent)
                     do  try(parseRightParent >>= return . return e)
                         <|> (genNewError (laset) ([TokRightParent]) >>= return . (checkError e))
                  
                  <|> (number >>= return . Right . num)
                  <|> (cleanEntry laset >>= return . Left . return . newNumberError)
                  <?> "numeros"


verifyBinError _  (Left xs) (Left ys) = Left (xs ++ ys)
verifyBinError _  (Left xs) _         = Left xs
verifyBinError _  _         (Left ys) = Left ys
verifyBinError op (Right x) (Right y) = Right(op x y) 
