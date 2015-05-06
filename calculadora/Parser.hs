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
                                 , waitedTok :: WaitedToken
                                 , actualTok :: Token
                                 }
                  | EmptyError   { line    :: P.Line
                                 , column  :: P.Column
                                 }

               deriving (Read)

data WaitedToken =  Operator
                  | Number
                  | TokenRP
                  deriving(Read)

instance Show WaitedToken where
  show Operator = "operador"
  show Number   = "numero"
  show TokenRP  = "paréntesis derecho"

instance Show MyParseError where
  show (MyParseError line column wt at) = "Error en la línea " ++ show line ++ ", columna " ++ show column ++ ": Esperaba " ++ show wt ++ " en vez de " ++ show at
  show (EmptyError   line column)       = "Error en la línea " ++ show line ++ ", columna " ++ show column ++ ": No se permiten expresiones vacías"

newEmptyError  pos          = EmptyError   { line = P.sourceLine pos, column = P.sourceColumn pos }             
newParseError  msg (e, pos) = MyParseError { line = P.sourceLine pos, column = P.sourceColumn pos, waitedTok = msg, actualTok = e }

genNewError :: Parsec [TokenPos] () (Token) -> WaitedToken -> Parsec [TokenPos] () (MyParseError)
genNewError laset msg = do  pos <- cleanEntry laset
                            return (newParseError msg pos)

cleanEntry :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (TokenPos)
cleanEntry laset = do pos <- getPosition
                      e   <- (lookAhead(parseComma) <|> lookAhead(parseEnd) <|> parseAnyToken)
                      panicMode laset
                      return ((e, pos))

parseExpr = expr parseEnd

parseListExpr :: Parsec [TokenPos] () (Either [MyParseError] [Integer])
parseListExpr = do e <- expr (parseEnd <|> parseComma)
                   do      parseComma >> parseListExpr >>= return . (verifyBinError (:) e)
                      <|> (parseEnd   >> return (fmap (return) e))
                    
expr :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] Integer)
expr laset =  do lookAhead(laset)
                 pos <- getPosition
                 return (Left (return (newEmptyError pos)))

              <|> do t <- term' (laset <|> parsePlus <|> parseMinus) 
                     do (lookAhead(laset) >> return t)
                        <|> (parsePlus  AP.*> expr laset    >>= return . (verifyBinError (+) t))
                        <|> (parseMinus AP.*> expr laset    >>= return . (verifyBinError (-) t))
                        <|> (genNewError (laset) (Operator) >>= return . (checkError t))
                            
                     <?> "expresion"

concatError (Left errors) (Left errors') = (Left (errors ++ errors'))
concatError (Left errors) _              = (Left errors)

checkError (Left xs) s = Left (s:xs)
checkError _         s = Left [s] 

term' :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] Integer)
term' laset = do p <- factor (laset <|> parseSlash <|> parseStar)
                 do (lookAhead(laset) >> return p)
                    <|> (parseSlash AP.*> term' laset   >>= return . (verifyBinError (div) p))
                    <|> (parseStar  AP.*> term' laset   >>= return . (verifyBinError (*)   p))
                    <|> (genNewError (laset) (Operator) >>= return . (checkError p))
                    <?> "termino"

panicMode until = manyTill parseAnyToken (lookAhead until)

factor :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (Either [MyParseError] Integer)
factor laset = do do parseLeftParent
                     e <- expr (parseEnd <|> parseComma <|> parseRightParent)
                     do  try(parseRightParent >>= return . return e)
                         <|> (genNewError (laset) (TokenRP) >>= return . (checkError e))
                  
                  <|> (number >>= return . Right . num)
                  <|> (genNewError laset Number >>= return . Left . return)
                  <?> "numeros"


verifyBinError _  (Left xs) (Left ys) = Left (xs ++ ys)
verifyBinError _  (Left xs) _         = Left xs
verifyBinError _  _         (Left ys) = Left ys
verifyBinError op (Right x) (Right y) = Right(op x y) 
