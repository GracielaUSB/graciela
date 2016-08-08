{-# LANGUAGE CApiFFI           #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where
--------------------------------------------------------------------------------
-- import           LLVM.Codegen
-- import           ASTtype
import           AST.Program
import           Graciela
import           Lexer
import           Parser.Program
import           SymbolTable
import           Token
import           Treelike
import           Type
import           TypeError
--------------------------------------------------------------------------------
import           Control.Monad          (unless, void, when, (>=>))
import           Control.Monad.Except   (ExceptT, runExceptT)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State    (runState)
import           Data.Foldable          (toList)
import           Data.List              (nub)
import           Data.Map.Strict        (showTree)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import qualified Data.Sequence          as Seq (null)
import           Data.Set               (empty)
import           Data.String.Utils      (replace)
import           Data.Text              (Text, unpack)
import           Data.Text.IO           (readFile)

import           LLVM.General.Context   (withContext)
import           LLVM.General.Module    (File (..), Module, withModuleFromAST,
                                         writeLLVMAssemblyToFile,
                                         writeObjectToFile)
import           LLVM.General.Target    (withHostTargetMachine)

import           Prelude                hiding (readFile)

import           System.Console.GetOpt  (ArgDescr (..), ArgOrder (..),
                                         OptDescr (..), getOpt, usageInfo)
import           System.Directory       (doesFileExist)
import           System.Environment     (getArgs)
import           System.Exit            (ExitCode (..), die, exitSuccess)
import           System.FilePath.Posix  (replaceExtension, takeExtension)
import           System.Info            (os)
import           System.Process         (readProcess, readProcessWithExitCode)

import           Text.Megaparsec        (ParsecT, runParser, runParserT,
                                         sourceColumn, sourceLine, parseErrorPretty)
import           Text.Megaparsec.Error  (ParseError, errorPos)
--------------------------------------------------------------------------------
-- Options -----------------------------
version :: String
version = "graciela 0.1.0.0"

help :: String
help = usageInfo message options

message :: String
message = "uso: graciela [OPCIÓN]... [ARCHIVO]"

data Options = Options
    { optHelp     :: Bool
    , optVersion  :: Bool
    , optErrors   :: Maybe Int
    , optExecName :: String
    , optAST      :: Bool
    , optSTable   :: Bool
    }

defaultOptions   = Options
    { optHelp     = False
    , optVersion  = False
    , optErrors   = Nothing
    , optExecName = "a.out"
    , optAST      = False
    , optSTable   = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['?'] ["ayuda"]
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
                    _  -> opts { optExecName = fileName }
                ) "NOMBRE")
        "Nombre del ejecutable"
    , Option ['s'] ["symtable"]
        (NoArg (\opts -> opts { optSTable = True }))
        "Imprime la tabla de simbolos por stdin"
    , Option ['a'] ["ast"]
        (NoArg (\opts -> opts { optAST = True }))
        "Imprime el AST por stdin"
    ]

opts :: IO (Options, [String])
opts = do
    args <- getArgs
    case getOpt Permute options args of
        (flags, rest, []) ->
            return (foldl (flip Prelude.id) defaultOptions flags, rest)
        (_, _, errs) ->
            ioError (userError (concat errs <> help))

-- Processing --------------------------
-- concatLexPar :: ParsecT Text () Identity (Either ParseError (Maybe Program), GracielaState)
-- concatLexPar = playParser <$> lexer


-- playParser :: [TokenPos] -> (Either ParseError (Maybe Program), GracielaState)
-- playParser inp = runStateParse program "" inp initialState


-- runStateParse :: Graciela (Maybe Program) -> String
--               -> [TokenPos]-> GracielaState
--               -> (Either ParseError (Maybe Program), GracielaState)
-- runStateParse p sn inp init = runIdentity $ runStateT (runPT p () sn inp) init


-- liftError :: ExceptT String IO a -> IO a
-- liftError = runExceptT >=> either fail return


-- generateCode :: Module -> ExceptT String IO ()
-- generateCode m =
--     withHostTargetMachine $ \tm ->
--         liftError $ writeObjectToFile tm (File "prueba") m

-- play opts inp llName =
--   case runParser concatLexPar () "" inp of
--     Left err -> do
--         let msg  = head $ messageString $ head $ errorMessages err
--             col  = sourceColumn $ errorPos err
--             line = sourceLine   $ errorPos err
--         die $
--             "Error en la línea " <> show line <> ", columna " <> show col <>
--             ": Caracter Lexicográfico " <> show msg <> " inválido.\n"

--     Right (Left  err', _) ->
--         die $ "\nOcurrió un error en el proceso de análisis sintáctico " <> show err'

--     Right (Right (Just ast), st) ->
--         if Seq.null (_sTableErrorList st) && Seq.null (_synErrorList st)
--             then do
--                 -- putStrLn $drawST 0 $current $symbolTable st
--                 let symTable = _symbolTable st
--                 when (optSTable opts) $ do
--                     let types    = _typesTable st
--                     putStrLn $ drawTree $ toTree symTable
--                     putStrLn $ drawTree $ Node "Types" $fmap (leaf . show) $toList types
--                 when (optAST opts) $
--                     putStrLn . drawTree . toTree $ ast

--                 let (t, l) = runTVerifier symTable ast

--                 if Seq.null l then do
--                     version <- getOSVersion
--                     let newast = astToLLVM (toList $ _filesToRead st) t version

--                     withContext $ \context ->
--                         liftError $ withModuleFromAST context newast $ \m ->
--                             liftError $ writeLLVMAssemblyToFile
--                                 (File llName ) m
--                 else die $ drawTypeError (optErrors opts) l

--             else die $ drawState (optErrors opts) st
--             where
--                 {- Gets OSX version -}
--                 getOSVersion :: IO String
--                 getOSVersion = case os of
--                     "darwin" ->
--                         readProcess "/usr/bin/sw_vers" ["-productVersion"] []
--                     _        -> return ""
--     Right (Right Nothing, st) -> die $ drawState (optErrors opts) st

-- Main --------------------------------
main :: IO ()
main = do
    (options, args) <- opts

    when (optVersion options) $ do
        putStrLn version
        exitSuccess
    when (optHelp options) $ do
        putStr help
        exitSuccess
    when (null args) $
        die "ERROR: No se indicó un archivo."

    let fileName = head args

    doesFileExist fileName >>= \x -> unless x
        (die $ "ERROR: El archivo `" <> fileName <> "` no existe.")

    unless (takeExtension fileName == ".gcl")
        (die "ERROR: El archivo no tiene la extensión apropiada, `.gcl`.")

    let execName = optExecName options
    let llName = if execName == "a.out"
        then "a.ll"
        else execName <> ".ll"

    source <- readFile fileName

    -- play options source llName

    {-/ Testing -}
    let Right ets = runParser lexer fileName source
    let (r, s) = runState (runParserT program (unpack source) ets) initialState

    case r of 
        Right program -> do 
            when (optAST options) $ putStrLn $ drawTree $ toTree  program
            when (optSTable options) $ do 
                putStrLn $ drawTree $ toTree $ fst $ _symbolTable s
                putStrLn $ drawTree $ Node "Types" $ fmap (leaf . show) $ toList $ _typesTable s
        Left e -> putStr $ parseErrorPretty e
        _ -> undefined

    putStr . unlines . toList . fmap ((++"\n").show) . _synErrorList $ s

    {- Testing /-}

    -- compileLL llName execName


compileLL :: String -> String -> IO ()
compileLL llName execName = void $ do
    (exitCode, _out, _errs) <-
        readProcessWithExitCode clang ["-o", execName, llName, lib] ""

    case exitCode of
        ExitSuccess ->
            void $ readProcess "rm" [llName] ""
        ExitFailure _ ->
            die "clang error"

    where
        lib  = case os of
            "darwin"  -> "/usr/local/lib/graciela-lib.so"
            "linux"   -> "/usr/local/lib/graciela-lib.so"
            "windows" -> undefined
        clang = case os of
            "darwin" -> "/usr/local/bin/clang-3.5"
            "linux"  -> "clang-3.5"
            "windows" -> undefined
