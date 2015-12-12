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
import MyTypeError
import Text.Parsec
import Data.Set (empty)
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


withTargetMipsMachine :: (TargetMachine -> IO ()) -> IO ()
withTargetMipsMachine f = do
    initializeAllTargets
    t <- liftError $ lookupTarget (Just "mips") ("x86-unknown-linux-gnu")
    withTargetOptions $ \options -> withTargetMachine (fst t) (snd t) 
               "mips32" empty options Reloc.Default CodeModel.Default CodeGenOpt.Default f
  

generateCode :: Module -> ExceptT String IO ()
generateCode m =
  do withDefaultTargetMachine $ \tm ->
      liftError $ writeObjectToFile tm (File "prueba") m


play :: T.Text -> IO ()
play inp = 

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
                                        astToLLVM (SET.toList $ filesToRead st) $ fst $ runTVerifier (symbolTable st) ast
                                  withContext $ \context ->
                                      liftError $ withModuleFromAST context newast $ \m -> do
                                      --liftError $ generateCode m
                                      liftError $ writeLLVMAssemblyToFile (File "prueba.bc") m
                    else 
                        putStrLn $ drawState st

          ; (Right  _         , st) -> putStrLn $ drawState st
          }
    }


main :: IO ()
main = do args <- getArgs 
          s <- TIO.readFile (head args)
          play s
