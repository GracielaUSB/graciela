module Main where
  
import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Applicative as AP
import qualified Data.Text.IO        as TIO
import qualified Data.Text           as T
import Control.Monad.State           as ST
import Control.Monad.Identity
import System.Environment
import TokenParser
import SymbolTable
import Text.Parsec
import ASTtype
import VerTypes
import Parser
import Lexer
import State
import AST

concatLexPar = playParser AP.<$> lexer

runStateParse p sn inp init = runIdentity $ ST.runStateT (runPT p () sn inp) init

playParser inp = runStateParse (program) "" (inp) (initialState)

playLexer inp = putStrLn $ show $ runParser (lexer) () "" (inp)

play inp = case (runParser (concatLexPar) () "" (inp)) of
            		  { Left  err -> putStrLn $ "Ocurrio un error en el proceso de parseo " ++ (show err)
            		  ; Right par -> case par of
           		                     { (Left  err', _ ) -> putStrLn $ "Ocurrio un error lexicografico " ++ (show err')
                                   ; (Right (Just ast) , st) -> putStrLn $ show $ runTVerifier (symbolTable st) ast
                                   --; (Right (ast) , st) -> putStrLn $ show $ st
                                                         -- let check = fmap verTypeAST ast 
														                             -- in case check of
														                             -- { (Nothing)  -> putStrLn "El arbol no se creo, esta malo"
														                             -- ; (Just ast) -> putStrLn "" -- drawAST 0 ast
														                             -- } 
                                   }
                  }

main = do args <- getArgs 
          s <- TIO.readFile (head args)
          play s
