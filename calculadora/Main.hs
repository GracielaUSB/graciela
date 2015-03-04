module Main where
  
import Text.Parsec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative as AP
import Parser
import Lexer
import Token

playLexer inp = case runParser (lexer) () "" (inp) of
            { Left  err -> putStrLn $ "Ocurrio un error en el proceso de parseo " ++ (show err)
            ; Right par -> putStrLn (show par)
            }

playParser inp = runParser (parseListExpr) () "" (inp)

concatLexPar = playParser AP.<$> lexer

play inp = case runParser (concatLexPar) () "" (inp) of
            { Left  err -> putStrLn $ "Ocurrio un error en el proceso de parseo " ++ (show err)
            ; Right par -> case par of
                             { Left  err' -> putStrLn $ "Ocurrio un error lexicografico " ++ (show err')
                             ; Right par' -> putStrLn $ show par'
                             }
            }
                           
parseFromFile :: FilePath -> IO ()
parseFromFile arch = do  s <- TIO.readFile arch
                         play s
