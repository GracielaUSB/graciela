module Lexer where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import Token

pPlus         = oneOf "+\43"
pMinus        = oneOf "-\45"
pStar         = oneOf "*\215"
pSlash        = oneOf "/\247"
pComma        = oneOf ",\44"
pLeftParent   = oneOf "(\40"
pRightParent  = oneOf ")\41"

lexer :: Parsec T.Text () ([TokenPos])
lexer = do pos <- getPosition    
           do  (eof >> spaces >> return ([(TokEnd, pos)]))
               <|> (do tok <- (   (pPlus        >> spaces >> return (TokPlus))
                              <|> (pMinus       >> spaces >> return (TokMinus))
                              <|> (pStar        >> spaces >> return (TokStar))
                              <|> (pSlash       >> spaces >> return (TokSlash))
                              <|> (pComma       >> spaces >> return (TokComma))
                              <|> (pLeftParent  >> spaces >> return (TokLeftParent))
                              <|> (pRightParent >> spaces >> return (TokRightParent))
                              <|> ((many1 digit)  AP.<* spaces >>= return . (TokInteger . read))
                              <|> (anyChar        AP.<* spaces >>= return . (TokError . T.singleton)))
                       fmap ((tok, pos) :) lexer)
