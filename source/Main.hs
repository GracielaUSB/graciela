module Main where
  
import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Applicative      as AP
import qualified Data.Text.IO             as TIO
import qualified Data.Text                as T
import Control.Monad.State                as ST
import Control.Monad.Identity
import System.Environment
import TokenParser
import SymbolTable
import Text.Parsec
import MyTypeError
import Expression
import VerTypes
import ASTtype
import Parser
import State
import Lexer
import AST
import Codegen

concatLexPar = playParser AP.<$> lexer


runStateParse p sn inp init = runIdentity $ ST.runStateT (runPT p () sn inp) init


playParser inp = runStateParse (exprLevel1 parseSemicolon parseSemicolon) "" inp initialState


playLexer inp = putStrLn $ show $ runParser lexer () "" inp


play inp = case (runParser (concatLexPar) () "" (inp)) of
            		  { Left  err -> putStrLn $ "Ocurrio un error lexicografico " ++ (show err)
            		  ; Right par -> case par of
           		                     { (Left  err', _ ) -> putStrLn $ "Ocurrio un error en el proceso de parseo " ++ (show err')
                                   ; (Right (Just ast) , st) -> do putStrLn $ drawState st
                                                                   putStrLn $ show $ execCodegen $ astToInstr $ fst $ runTVerifier (symbolTable st) ast
                                                                    
                                   ; (Right  _         , st) -> putStrLn $ drawState st
                                   }
                  }


main = do args <- getArgs 
          s <- TIO.readFile (head args)
          play s
