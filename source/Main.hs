module Main where
  
import qualified LLVM.General.CodeGenOpt  as CodeGenOpt
import qualified LLVM.General.Relocation  as Reloc
import qualified LLVM.General.CodeModel   as CodeModel
import qualified Control.Applicative      as AP
import qualified Data.Foldable            as DF
import qualified Data.Text.IO             as TIO
import qualified Data.Text                as T
import qualified Data.Set                 as SET
import Control.Monad.State                as ST
import Control.Monad.Identity
import Control.Monad.Except
import LLVM.General.Context
import LLVM.General.Target
import LLVM.General.Module
import System.Environment
import Text.Parsec.Error
import Data.String.Utils
import System.Directory
import MyTypeError
import Text.Parsec
import Data.Set (empty)
import Data.List
import ASTtype
import Codegen
import Parser
import State
import Lexer
import Token
import Type 
import AST


concatLexPar :: ParsecT T.Text () Identity (Either ParseError (Maybe (AST Type)), ParserState)
concatLexPar = playParser AP.<$> lexer


playLexer :: T.Text -> IO ()
playLexer inp = putStrLn $ show $ runParser lexer () "" inp


playParser :: [TokenPos] -> (Either ParseError (Maybe (AST Type)), ParserState)
playParser inp = runStateParse (program) "" inp initialState


runStateParse p sn inp init = runIdentity $ ST.runStateT (runPT p () sn inp) init


liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

generateCode :: Module -> ExceptT String IO ()
generateCode m =
  do withDefaultTargetMachine $ \tm ->
      liftError $ writeObjectToFile tm (File "prueba") m


play inp fileName = 

    case (runParser (concatLexPar) () "" (inp)) of
    { Left err -> 

          do let msg  = head $ messageString $ head $ errorMessages err
             let col  = sourceColumn $ errorPos err
             let line = sourceLine   $ errorPos err
             putStrLn $ "\nError en la línea " ++ show line ++ ", columna " ++ show col ++
                        ": Caracter Lexicografico " ++ show msg ++ " inválido.\n"

    ; Right par -> 
          case par of
          { (Left  err', _ ) -> 
                 putStrLn $ "\nOcurrio un error en el proceso de parseo " ++ (show err')
          
                 --do let msg  = head $ messageString $ head $ errorMessages err'
                 --   let col  = sourceColumn $ errorPos err'
                 --   let line = sourceLine   $ errorPos err'
                 --   putStrLn $ "\nError en la línea " ++ show line ++ ", columna " ++ show col ++ show msg ++ ".\n"

          ; (Right (Just ast) , st) -> 
                 let lErrType = DF.toList $ sTableErrorList st
                     lErrSyn  = DF.toList $ synErrorList    st
                 in if (null lErrType) && (null lErrSyn) then
                        do let (t, l) = runTVerifier (symbolTable st) ast 
                               l'     = DF.toList l 

                           if not $ null l' then 
                               putStrLn $ drawTypeError l'
                           else 
                               do let newast = 
                                        astToLLVM (SET.toList $ filesToRead st) $ t
                                  withContext $ \context ->
                                      liftError $ withModuleFromAST context newast $ \m -> do
                                      --liftError $ generateCode m
                                      liftError $ writeLLVMAssemblyToFile 
                                          (File $ (init . init . init . init $ fileName) ++ ".bc") m
                    else 
                        putStrLn $ drawState st

          ; (Right  _         , st) -> putStrLn $ drawState st
          }
    }



play2 inp fileName = 

    case (runParser (concatLexPar) () "" (inp)) of
    { Left err -> 

          do let msg  = head $ messageString $ head $ errorMessages err
             let col  = sourceColumn $ errorPos err
             let line = sourceLine   $ errorPos err
             putStrLn $ "\nError en la línea " ++ show line ++ ", columna " ++ show col ++
                        ": Caracter Lexicografico " ++ show msg ++ " inválido.\n"

    ; Right par -> 
          case par of
          { (Left  err', _ ) -> 
                 putStrLn $ "\nOcurrio un error en el proceso de parseo " ++ (show err')
          
                 --do let msg  = head $ messageString $ head $ errorMessages err'
                 --   let col  = sourceColumn $ errorPos err'
                 --   let line = sourceLine   $ errorPos err'
                 --   putStrLn $ "\nError en la línea " ++ show line ++ ", columna " ++ show col ++ show msg ++ ".\n"

          ; (Right (Just ast) , st) -> 
                 let lErrType = DF.toList $ sTableErrorList st
                     lErrSyn  = DF.toList $ synErrorList    st
                 in if (null lErrType) && (null lErrSyn) then
                        do let (t, l) = runTVerifier (symbolTable st) ast 
                               l'     = DF.toList l 

                           if not $ null l' then 
                               putStrLn $ drawTypeError2 l'
                           else 
                               do let newast = 
                                        astToLLVM (SET.toList $ filesToRead st) $ t
                                  withContext $ \context ->
                                      liftError $ withModuleFromAST context newast $ \m -> do
                                      --liftError $ generateCode m
                                      liftError $ writeLLVMAssemblyToFile 
                                          (File $ (init . init . init . init $ fileName) ++ ".bc") m
                    else 
                        putStrLn $ drawState2 st

          ; (Right  _         , st) -> putStrLn $ drawState2 st
          }
    }


main :: IO ()
main = do 
    args <- getArgs 
    let fileName = head args

    check <- doesFileExist fileName

    case isSuffixOf ".gcl" fileName of      
    { True  -> case check of      
               { True  -> case last args of
                          { "0" -> do s <- TIO.readFile fileName
                                      play s fileName

                          ; "1" -> do s <- TIO.readFile fileName
                                      play2 s fileName
                          }

               ; False -> putStrLn $ "\nERROR: El archivo no existe en el directorio.\n"  
               }

    ; False -> putStrLn $ "\nERROR: El archivo no posee la extensión. \".gcl\" \n"  
    }


 

