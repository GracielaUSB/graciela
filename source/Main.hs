module Main where
  
import Text.Parsec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Control.Applicative as AP
import Parser
import Lexer
import Token
import System.Environment

playLexer inp = case runParser (lexer) () "" (inp) of
            { Left  err -> putStrLn $ "Ocurrio un error en el proceso de parseo " ++ (show err)
            ; Right par -> putStrLn (show par)
            }

playParser inp = runParser (program) () "" (inp)

concatLexPar = playParser AP.<$> lexer

play inp = case runParser (concatLexPar) () "" (inp) of
            { Left  err -> putStrLn $ "Ocurrio un error en el proceso de parseo " ++ (show err)
            ; Right par -> case par of
                             { Left  err' -> putStrLn $ "Ocurrio un error lexicografico " ++ (show err')
                             ; Right par' -> case par' of 
                                                { Left  par''' -> putStr $ concatMap (\s -> (show s) ++ "\n") par'''
                                                ; Right par''' -> putStr $ show $ par''' --concatMap (\s -> (show s) ++ "\n") par'''
                                                }

                             }
            }
                           
main = do args <- getArgs 
          s <- TIO.readFile (head args)
          play s
