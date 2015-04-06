module Error where

import Text.Parsec
import qualified Text.Parsec.Pos as P
import Token

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
                  | TokenRB
                  | Comma
                  | Final
                  deriving(Read)

instance Show WaitedToken where
  show Operator = "operador"
  show Number   = "numero"
  show TokenRP  = "paréntesis derecho"
  show Comma    = "coma"
  show Final    = "final de archivo"
  show TokenRB  = "corchete derecho"
  
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
                      e   <- (lookAhead(laset) <|> lookAhead(parseEnd) <|> parseAnyToken)
                      panicMode laset
                      return ((e, pos))

panicMode until = manyTill parseAnyToken (lookAhead until)

concatError (Left errors) (Left errors') = (Left (errors ++ errors'))
concatError (Left errors) _              = (Left errors)

checkError (Left xs) s = Left (xs ++ [s])
checkError _         s = Left [s] 

verifyBinError _  (Left xs) (Left ys) = Left (xs ++ ys)
verifyBinError _  (Left xs) _         = Left xs
verifyBinError _  _         (Left ys) = Left ys
verifyBinError op (Right x) (Right y) = Right(op x y) 
