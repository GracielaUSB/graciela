module Error where

import Text.Parsec
import qualified Text.Parsec.Pos as P
import TokenParser
import Token
import Data.Monoid
import Location
import State
import Control.Monad.State as ST
import MyParseError

newEmptyError  pos          = EmptyError   { loc = Location (P.sourceLine pos) (P.sourceColumn pos) (P.sourceName pos)                                 }
newParseError  msg (e, pos) = MyParseError { loc = Location (P.sourceLine pos) (P.sourceColumn pos) (P.sourceName pos), waitedTok = msg, actualTok = e }

genNewError :: MyParser (Token) -> WaitedToken -> MyParser ()
genNewError laset msg = do  pos <- cleanEntry laset
                            ST.modify $ addError $ newParseError msg pos
                            return ()

genNewEmptyError :: MyParser ()
genNewEmptyError = do  pos <- getPosition
                       ST.modify $ addError $ newEmptyError pos
                       return ()

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
