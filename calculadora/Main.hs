module Main where
  
import Text.Parsec
import qualified Data.Text as T
import Control.Applicative as AP
import Parser
import Lexer
import Token

playLexer inp = runParser (lexer) () "" (T.pack inp)                     

playParser inp = runParser (parseExpr) () "" (inp)

concatLexPar = playParser AP.<$> lexer

play inp = case runParser (concatLexPar) () "" (T.pack inp) of
            { Left  err -> putStrLn $ "Ocurrio un error en el proceso de parseo " ++ (show err)
            ; Right par -> case par of
                             { Left  err' -> putStrLn $ "Ocurrio un error lexicografico " ++ (show err')
                             ; Right par' -> putStrLn $ show par'
                             }
            }
                           
