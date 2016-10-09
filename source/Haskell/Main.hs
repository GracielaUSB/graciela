{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where
--------------------------------------------------------------------------------
import           AST.Program
import           AST.Type
import           Common
import           Error
import           Lexer
import           LLVM.Program
import           Parser.Monad
import           Parser.Program
import           Parser.State
import           SymbolTable
import           Token
import           Treelike
--------------------------------------------------------------------------------
import           Control.Lens               ((^.))
import           Control.Monad              (unless, void, when, (>=>))
import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.State  (runState)
import           Data.Foldable              (toList)
import           Data.List                  (nub)
import           Data.Map.Strict            (showTree)
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Sequence              as Seq (null)
import           Data.Set                   (empty)
import           Data.Text                  (Text, unpack)
import           Data.Text.IO               (readFile)

import           LLVM.General.Context       (withContext)
import           LLVM.General.Module        (File (..), Module,
                                             withModuleFromAST,
                                             writeLLVMAssemblyToFile,
                                             writeObjectToFile)
import           LLVM.General.Target        (withHostTargetMachine)

import           Prelude                    hiding (lex, readFile)

import           System.Console.GetOpt      (ArgDescr (..), ArgOrder (..),
                                             OptDescr (..), getOpt, usageInfo)
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           System.Exit                (ExitCode (..), die, exitSuccess)
import           System.FilePath.Posix      (replaceExtension, takeExtension)
import           System.Info                (os)
import           System.IO                  (hPutStr, stderr)
import           System.Process             (readProcess,
                                             readProcessWithExitCode)

import           Text.Megaparsec            (ParsecT, parseErrorPretty,
                                             sourceColumn, sourceLine)
import           Text.Megaparsec.Error      (ParseError, errorPos)
--------------------------------------------------------------------------------
-- Options -----------------------------
version :: String
version = "graciela 0.1.0.0"

help :: String
help = usageInfo message options

message :: String
message = "uso: graciela [OPCIÓN]... [ARCHIVO]"

data Options = Options
  { optHelp         :: Bool
  , optVersion      :: Bool
  , optErrors       :: Maybe Int
  , optOutName      :: Maybe String
  , optAST          :: Bool
  , optSTable       :: Bool
  , optOptimization :: String
  , optAssembly     :: Bool
  , optLLVM         :: Bool }

defaultOptions      = Options
  { optHelp         = False
  , optVersion      = False
  , optErrors       = Nothing
  , optOutName      = Nothing
  , optAST          = False
  , optSTable       = False
  , optOptimization = ""
  , optAssembly     = False
  , optLLVM         = False }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['?', 'h'] ["ayuda"]
    (NoArg (\opts -> opts { optHelp = True }))
    "Muestra este mensaje de ayuda"
  , Option ['v'] ["version"]
    (NoArg (\opts -> opts { optVersion = True }))
    "Muestra la versión del compilador"
  , Option ['e'] ["errores"]
    (ReqArg (\ns opts -> case reads ns of
      [(n,"")] -> opts { optErrors = Just n }
      _        -> error "Valor inválido en el argumento de `errores`"
    ) "ENTERO")
    "Limita el número de errores mostrados"
  , Option ['o'] ["nombre"]
    (ReqArg (\fileName opts -> case fileName of
      "" -> error "Valor inválido en el argumento de `-o`"
      _  -> opts { optOutName = Just fileName }
    ) "NOMBRE")
    "Nombre del ejecutable"
  , Option ['s'] ["symtable"]
    (NoArg (\opts -> opts { optSTable = True }))
    "Imprime la tabla de simbolos por stdin"
  , Option ['a'] ["ast"]
    (NoArg (\opts -> opts { optAST = True }))
    "Imprime el AST por stdin"
  , Option ['S'] ["assembly"]
    (NoArg (\opts -> opts { optAssembly = True }))
    "Generar codigo ensamblador"
  , Option ['L'] ["llvm"]
    (NoArg (\opts -> opts { optLLVM = True }))
    "Generar codigo intermedio LLVM"
  , Option ['O'] ["optimization"]
    (ReqArg (\level opts -> opts { optOptimization = "-O" <> level }) "NIVEL")
      "Niveles de optimizacion\n\
      \-O0 Sin optimizacion\n\
      \-O1 poca optimizacion\n\
      \-O2 optimizacion por defecto\n\
      \-O3 optimizacion agresiva"
  ]

opts :: IO (Options, [String])
opts = do
  args <- getArgs
  case getOpt Permute options args of
    (flags, rest, []) ->
      return (foldl (flip Prelude.id) defaultOptions flags, rest)
    (_, _, errs) ->
      ioError (userError (concat errs <> help))

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

-- Main --------------------------------
main :: IO ()
main = do
  (options, args) <- opts

  -- Print Version
  when (optVersion options) $ do
    putStrLn version
    exitSuccess

  -- Print command options
  when (optHelp options) $ do
    putStr help
    exitSuccess

  -- Print "No file" Error
  when (null args) $
    die "ERROR: No se indicó un archivo."

  -- Get the name of source file
  let fileName = head args

  doesFileExist fileName >>= \x -> unless x
    (die $ "ERROR: El archivo `" <> fileName <> "` no existe.")

  unless (takeExtension fileName == ".gcl")
    (die "ERROR: El archivo no tiene la extensión apropiada, `.gcl`.")

  -- Read the source file
  source <- readFile fileName

  let
    tokens = lex fileName source
    (r, state) = runParser program fileName (initialState fileName) tokens

  if null (state ^. errors)
    then case r of
      Right program@Program { name } -> do

        {-Print AST-}
        when (optAST options) $ do
          putStrLn . drawTree . toTree $ program
          exitSuccess
        {-Print Symbol Table-}
        when (optSTable options) $ do
          putStrLn . drawTree . toTree . defocus $ state ^. symbolTable
          -- putStrLn . drawTree . Node "Types" . toList $
          --   leaf . show <$> state ^. typesTable
          exitSuccess

        {- Generate LLVM AST -}
        let
          files = toList $ state ^. filesToRead
          -- types = state ^. typesTable

        newast <- programToLLVM files program

        let
          lltName = case optOutName options of
            Nothing -> "a.t.ll"
            Just n  -> n <> ".t.ll"

        {- And write it as IR on a ll file -}
        withContext $ \context ->
          liftError . withModuleFromAST context newast $ \m -> liftError $
            writeLLVMAssemblyToFile (File lltName) m

        let
          assembly
            | optLLVM options     = ["-S", "-emit-llvm"]
            | optAssembly options = ["-S"]
            | otherwise           = []
          outName = case optOutName options of
            Just outName' -> outName'
            Nothing
              | optLLVM options     -> unpack name <> ".ll"
              | optAssembly options -> unpack name <> ".s"
              | otherwise           -> unpack name
          args = [optOptimization options]
              <> assembly
              <> [lltName]
              <> ["-o", outName]
              <> [lib         | not $ optLLVM options || optAssembly options]
              <> [abstractLib | not $ optLLVM options || optAssembly options]
        (exitCode, out, errs) <- readProcessWithExitCode clang args ""

        putStr out
        hPutStr stderr errs

        -- void $ readProcess "rm" [lltName] ""

        case exitCode of
          ExitSuccess ->
            pure ()
          ExitFailure _ ->
            die "clang error"

      Left message -> putStrLn $ prettyError message

    else
      {- If any errors occurred during Parsing, they will be printed here-}
      -- mapM_ print (state ^. errors)
      hPutStr stderr . unlines . mTake (optErrors options) . toList $
        prettyError <$> state ^. errors

  where
    mTake Nothing  xs = xs
    mTake (Just n) xs = take n xs
    lib  = case os of
      "darwin"  -> "/usr/local/lib/graciela-lib.so"
      "linux"   -> "/usr/local/lib/graciela-lib.so"
      "windows" -> undefined
    abstractLib  = case os of
      "darwin"  -> "/usr/local/lib/graciela-abstract-lib.so"
      "linux"   -> "/usr/local/lib/graciela-abstract-lib.so"
      "windows" -> undefined
    clang = case os of
      "darwin"  -> "/usr/local/bin/clang-3.5"
      "linux"   -> "clang-3.5"
      "windows" -> undefined
