{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
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
import           OS
import           Parser.Monad
import           Parser.Program
import           Parser.State
import           SymbolTable
import           Token
import           Treelike
--------------------------------------------------------------------------------
import           Control.Lens               ((^.))
import           Control.Monad.Identity     (Identity, runIdentity)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.State  (runState)
import           Data.Foldable              (toList)
import           Data.List                  (nub)
import           Data.Map.Strict            (showTree)
import           Data.Maybe                 (fromMaybe)

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
import           System.Directory           (doesFileExist, removeFile)
import           System.Environment         (getArgs)
import           System.Exit                (ExitCode (..), die, exitFailure,
                                             exitSuccess)
import           System.FilePath.Posix      (replaceExtension, takeExtension)
import           System.IO                  (stderr)
import           System.Process             (readProcess,
                                             readProcessWithExitCode)
import           Text.Megaparsec            (ParsecT, parseErrorPretty,
                                             sourceColumn, sourceLine)
import           Text.Megaparsec.Error      (ParseError, errorPos)
--------------------------------------------------------------------------------
-- Options -----------------------------
version :: String
version = "graciela 1.0.0.0"

help :: String
help = usageInfo message (rights options)

message :: String
message = "use: graciela [OPTIONS]... [FILE]"

data Options = Options
  { optHelp         :: Bool
  , optVersion      :: Bool
  , optErrors       :: Maybe Int
  , optOutName      :: Maybe String
  , optAST          :: Bool
  , optSTable       :: Bool
  , optOptimization :: String
  , optAssembly     :: Bool
  , optLLVM         :: Bool
  , optClang        :: String
  , optLibGraciela  :: String
  , optLibGracielaA :: String
  , optKeepTemp     :: Bool }

defaultOptions      = Options
  { optHelp         = False
  , optVersion      = False
  , optErrors       = Just 5
  , optOutName      = Nothing
  , optAST          = False
  , optSTable       = False
  , optOptimization = ""
  , optAssembly     = False
  , optLLVM         = False
  , optClang        = clang
  , optLibGraciela  = lib
  , optLibGracielaA = abstractLib
  , optKeepTemp     = False }
  where
    (clang, lib, abstractLib)
      | isLinux =
        ( "clang-3.5"
        , "/usr/lib/graciela/libgraciela.so"
        , "/usr/lib/graciela/libgraciela-abstract.so")
      | isMac =
        ( "/usr/local/bin/clang-3.5"
        , "/usr/local/lib/libgraciela.so"
        , "/usr/local/lib/libgraciela-abstract.so")
      | isWindows = internal $ "Windows not supported :("
      | otherwise = internal $ "Unknown OS, not supported :("

options :: [Both (OptDescr (Options -> Options))]
options =
  [ Right $ Option ['?', 'h'] ["help"]
    (NoArg (\opts -> opts { optHelp = True }))
    "Display this help message"
  , Right $ Option ['v'] ["version"]
    (NoArg (\opts -> opts { optVersion = True }))
    "Displays the version of the compiler"
  , Right $ Option ['e'] ["errors"]
    (ReqArg (\ns opts -> case reads ns of
      [(n,"")] -> opts { optErrors = Just n }
      _        -> error "Invalid argument for flag `errors`"
    ) "INTEGER")
    "Limit the number of displayed errors"
  , Right $ Option ['o'] ["out"]
    (ReqArg (\fileName opts -> case fileName of
      "" -> error "Invalid argument for flag `out`"
      _  -> opts { optOutName = Just fileName }
    ) "FILE NAME")
    "Set executable name"

  , Left $ Option ['s'] ["symtable"]
    (NoArg (\opts -> opts { optSTable = True }))
    "Print symtable on stdin"
  , Left $ Option ['a'] ["ast"]
    (NoArg (\opts -> opts { optAST = True }))
    "Print AST on stdin"
  , Left $ Option ['S'] ["assembly"]
    (NoArg (\opts -> opts { optAssembly = True }))
    "Generate assembler code"
  , Left $ Option ['L'] ["llvm"]
    (NoArg (\opts -> opts { optLLVM = True }))
    "Generate LLVM intermediate code"

  , Left $ Option [] ["clang"]
    (ReqArg (\executable opts -> opts { optClang = executable }
    ) "EXECUTABLE")
    "Set Clang to be used"
  , Left $ Option [] ["library"]
    (ReqArg (\library opts -> opts { optLibGraciela = library }
    ) "EXECUTABLE")
    "Set Library to be linked against"
  , Left $ Option [] ["abstract-library"]
    (ReqArg (\library opts -> opts { optLibGracielaA = library }
    ) "EXECUTABLE")
    "Set Abstract Library to be linked against"

  , Left $ Option ['K'] ["keep-temp"]
    (NoArg (\opts -> opts { optKeepTemp = True }))
    "Keep temporary llvm file"

  , Right $ Option ['O'] ["optimization"]
    (ReqArg (\level opts -> opts { optOptimization = "-O" <> level }) "LEVEL")
      "Optimization levels\n\
      \-O0 No optimization\n\
      \-O1 Somewhere between -O0 and -O2\n\
      \-O2 Moderate level of optimization which enables most optimizations\n\
      \-O3 Like -O2, except that it enables optimizations that take longer to perform or that may generate larger code (in an attempt to make the program run faster)."
  ]

opts :: IO (Options, [String])
opts = do
  args <- getArgs
  case getOpt Permute (eithers options) args of
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
  when (null args) . die $
    "\ESC[1;31m" <> "ERROR:" <> "\ESC[m" <> " No file was given."

  -- Get the name of source file
  let fileName = head args

  doesFileExist fileName >>= \x -> unless x
    (die $ "\ESC[1;31m" <> "ERROR:" <> "\ESC[m" <>
           " The file `" <> fileName <> "` does not exist.")

  unless (takeExtension fileName == ".gcl")
    (die $ "\ESC[1;31m" <> "ERROR:" <> "\ESC[m" <>
           " The file does not have the right extension, `.gcl`.")

  compile fileName options >>= \case
    Nothing  -> exitSuccess
    Just msg -> die (msg)

compile :: FilePath -> Options -> IO (Maybe String)
compile fileName options = do

  -- Read the source file
  source <- readFile fileName

  let
    (tokens, pragmas) = lex fileName source
    (r, state) = runParser program fileName (initialState pragmas) tokens

  if null (state ^. errors)
    then case r of
      Right program@Program { name } -> do
        if| optAST options -> do
            {-Print AST-}
            putStrLn . drawTree . toTree $ program
            pure Nothing

          | optSTable options -> do
            {-Print Symbol Table-}
            putStrLn . drawTree . toTree . defocus $ state ^. symbolTable
            pure Nothing

          | otherwise -> do
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
                  <> [l | l <- [math, lib, abstractLib]
                        , not $ optLLVM options || optAssembly options ]
            (exitCode, out, errs) <- readProcessWithExitCode clang args ""

            putStr out

            unless (optKeepTemp options) . void $
              removeFile lltName
              -- readProcess "rm" [lltName] ""

            case exitCode of
              ExitSuccess ->
                pure Nothing
              ExitFailure _ ->
                pure . Just $ "Clang error:\n" <> errs

      Left message -> do
        pure . Just $ prettyError message

    else do
      {- If any errors occurred during Parsing, they will be printed here-}

      let msg  = msg1 <> msg2
          msg1 = case optErrors options of
            Just n | length (state ^. errors) > n ->
              "Showing only the first " <> show n <> " errors of " <>
              show (length $ state ^. errors) <> " that were generated.\n"
            otherwise -> ""
          msg2 = unlines . mTake (optErrors options) . toList $
            prettyError <$> state ^. errors
      pure . Just $ msg

  where
    mTake Nothing  xs = take 3 xs
    mTake (Just n) xs = take n xs
    math = "-lm"
    [clang, lib, abstractLib] =
      ($ options) <$> [optClang, optLibGraciela, optLibGracielaA]
