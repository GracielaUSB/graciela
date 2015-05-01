module Error where

import Text.Parsec
import qualified Text.Parsec.Pos as P
import Token
import Data.Monoid
import Location

data MyParseError = MyParseError   { loc       :: Location
                                   , waitedTok :: WaitedToken
                                   , actualTok :: Token
                                   }
                  | EmptyError     { loc :: Location }
               deriving (Read)

data WaitedToken =  Operator
                  | Number
                  | TokenRP
                  | TokenRB
                  | Comma
                  | Final
                  | Program
                  | TokenOB
                  | TokenCB
                  | ProcOrFunc
                  | Colon
                  | IDError
                  deriving(Read)

instance Show WaitedToken where
  show Operator   = "operador"
  show Number     = "numero"
  show TokenRP    = "paréntesis derecho"
  show Comma      = "coma"
  show Final      = "final de archivo"
  show TokenRB    = "corchete derecho"
  show TokenOB    = "apertura de bloque"
  show TokenCB    = "final de bloque"
  show Program    = "program"
  show ProcOrFunc = "procedimiento o funcion"
  show Colon      = "dos puntos"
  show IDError    = "identificador"

instance Show MyParseError where
  show (MyParseError loc wt at)      = show loc ++ ": Esperaba " ++ show wt ++ " en vez de " ++ show at
  show (EmptyError   loc)            = show loc ++ ": No se permiten expresiones vacías"

newEmptyError  pos          = EmptyError   { loc = Location (P.sourceLine pos) (P.sourceColumn pos) (P.sourceName pos)                                 }
newParseError  msg (e, pos) = MyParseError { loc = Location (P.sourceLine pos) (P.sourceColumn pos) (P.sourceName pos), waitedTok = msg, actualTok = e }

genNewError :: Parsec [TokenPos] () (Token) -> WaitedToken -> Parsec [TokenPos] () (MyParseError)
genNewError laset msg = do  pos <- cleanEntry laset
                            return (newParseError msg pos)

cleanEntry :: Parsec [TokenPos] () (Token) -> Parsec [TokenPos] () (TokenPos)
cleanEntry laset = do pos <- getPosition
                      e   <- (lookAhead(laset) <|> lookAhead(parseEnd) <|> parseAnyToken)
                      panicMode laset
                      return ((e, pos))

panicMode until = manyTill parseAnyToken (lookAhead (until <|> parseEnd))

concatError (Left errors) (Left errors') = (Left (errors ++ errors'))
concatError (Left errors) _              = (Left errors)

checkError (Left xs) s = Left (reverse (s : xs))
checkError _         s = Left [s] 

verifyBinError _  (Left xs) (Left ys) = Left (xs `mappend` ys)
verifyBinError _  (Left xs) _         = Left xs
verifyBinError _  _         (Left ys) = Left ys
verifyBinError op (Right x) (Right y) = Right(op x y) 
