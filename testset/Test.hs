{-# LANGUAGE LambdaCase #-}

module Test (main) where

--------------------------------------------------------------------------------
import           Cola.Cola
import           Main                    (Options (..), defaultOptions)
import qualified Main                    as M (compile)
--------------------------------------------------------------------------------
import           Control.Monad           (forever, unless, void)
import           Data.Maybe              (isNothing)
import           Data.Semigroup          (Semigroup (..))
import           Prelude                 hiding (readFile)
import           System.Directory        (getCurrentDirectory, removeFile)
import           System.Exit             (ExitCode (..))
import           System.FilePath         ((-<.>), (<.>), (</>))
import           System.IO               (IOMode (..), SeekMode (..), hClose,
                                          hGetContents, hPutStrLn, hSeek,
                                          readFile, stderr, withFile)
import           System.IO.Temp          (openBinaryTempFile, openTempFile,
                                          withTempFile)
import           System.Process          (CreateProcess (..), StdStream (..),
                                          proc, readProcessWithExitCode)
import           Test.HUnit
import           Test.QuickCheck
import           Test.QuickCheck.Monadic hiding (assert)
import qualified Test.QuickCheck.Monadic as Q (assert)
--------------------------------------------------------------------------------

dir :: String
dir = "testset"

bin    name   = dir </> name </> name <.> "bin"
gcl    name   = dir </> name </> name <.> "gcl"
input  name n = dir </> name </> "in"  <> show n
output name n = dir </> name </> "out" <> show n
errput name n = dir </> name </> "err" <> show n

infixr 0 ~::
a ~:: b = runTestTT (a ~: b)

infixr 0 ~!::
_ ~!:: b = b
--------------------------------------------------------------------------------

compile :: String -> Assertion
compile name = do
  v1 <- M.compile (gcl name) defaultOptions
    { optOutName      = Just (bin name)
    , optLibGraciela  = "libgraciela.so"
    , optLibGracielaA = "libgraciela-abstract.so" }

  isNothing v1 @? ("Compilation failed:\n" <> show v1)
--------------------------------------------------------------------------------

prop_colas cs = monadicIO $ do
  (_, out, err) <- run $
    readProcessWithExitCode (bin "Cola") [] (show cs)
  Q.assert $ err == "" && out == correrSimulacion cs
--------------------------------------------------------------------------------

main = do
  -- Cola --
  "Compile Cola" ~::
    compile "Cola"
  "Run Cola" ~!::
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_colas
  removeFile (bin "Cola")

  -- Teoría de Conjuntos --
  "Compile TeoriaConjuntos" ~::
    compile "TeoriaConjuntos"
  "Run TeoriaConjuntos" ~::
    readProcessWithExitCode (bin "TeoriaConjuntos") [] "" >>=
    assertEqual "" (ExitSuccess, "", "")
  removeFile (bin "TeoriaConjuntos")
