module Main where
  
import Text.Parsec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Control.Applicative as AP
import Lexer
import Parser
import TokenParser
import System.Environment
import Tabla
import AST
import State
import Control.Monad.State as ST
import Control.Monad.Identity

concatLexPar = playParser AP.<$> lexer

runStateParse p sn inp init = runIdentity $ ST.runStateT (runPT p () sn inp) init

playParser inp = runStateParse (program) "" (inp) (initialState)

play inp = putStrLn $ show $ runParser (concatLexPar) () "" (inp)

main = do args <- getArgs 
          s <- TIO.readFile (head args)
          play s
