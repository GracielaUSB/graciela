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
import LLVM.General.Module
import LLVM.General.Context
import Control.Monad.Except

concatLexPar = playParser AP.<$> lexer

runStateParse p sn inp init = runIdentity $ ST.runStateT (runPT p () sn inp) init


playParser inp = runStateParse (program) "" inp initialState


playLexer inp = putStrLn $ show $ runParser lexer () "" inp

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

play inp = case (runParser (concatLexPar) () "" (inp)) of
            		  { Left  err -> putStrLn $ "Ocurrio un error lexicografico " ++ (show err)
            		  ; Right par -> case par of
           		                     { (Left  err', _ ) -> putStrLn $ "Ocurrio un error en el proceso de parseo " ++ (show err')
                                   ; (Right (Just ast) , st) -> 
                                      do let newast = runLLVM (emptyModule "hola bb") $ astToLLVM $ fst $ runTVerifier (symbolTable st) ast
                                         withContext $ \context ->
                                            liftError $ withModuleFromAST context newast $ \m -> do
                                              l <- moduleLLVMAssembly m
                                              putStrLn l
                                             
                                                                    
                                   ; (Right  _         , st) -> putStrLn $ drawState st
                                   }
                  }


main = do args <- getArgs 
          s <- TIO.readFile (head args)
          play s
