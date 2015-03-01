module Lexer where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import Token

pStar = oneOf "*\215"

lexer :: Parsec T.Text () ([TokenPos])
lexer = do pos <- getPosition    
           do  (eof >> spaces >> return ([(TokEnd, pos)]))
               <|> (do tok <- (   (char '+' >> spaces >> return (TokPlus))
                              <|> (char '-' >> spaces >> return (TokMinus))
                              <|> (pStar    >> spaces >> return (TokStar))
                              <|> (char '/' >> spaces >> return (TokSlash))
                              <|> (char ',' >> spaces >> return (TokComma))
                              <|> (char '(' >> spaces >> return (TokLeftParent))
                              <|> (char ')' >> spaces >> return (TokRightParent))
                              <|> ((many1 digit)  AP.<* spaces >>= return . (TokInteger . read))
                              <|> (anyChar        AP.<* spaces >>= return . (TokError . T.singleton)))
                       fmap ((tok, pos) :) lexer)
