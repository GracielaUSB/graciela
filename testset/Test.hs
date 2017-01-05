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

bin base name = dir </> base </> base <> name <.> "bin"
gcl base name = dir </> base </> base <> name <.> "gcl"

infixr 0 ~::
a ~:: b = runTestTT (a ~: b)

infixr 0 ~!::
_ ~!:: b = b
--------------------------------------------------------------------------------

compile :: String -> String -> Assertion
compile base name = do
  v1 <- M.compile (gcl base name) defaultOptions
    { optOutName      = Just (bin base name)
    , optLibGraciela  = "libgraciela.so" }

  isNothing v1 @? ("Compilation failed:\n" <> show v1)
--------------------------------------------------------------------------------

prop_cola cs = monadicIO $ do
  (_, out, err) <- run $
    readProcessWithExitCode (bin "Cola" "") [] (show cs)
  Q.assert $ err == "" && out == correrSimulacion cs

prop_cola_enlazada cs = monadicIO $ do
  (_, out, err) <- run $
    readProcessWithExitCode (bin "Cola" "Enlazada") [] (show cs)
  -- run $ do
  --   writeFile "_in"      (show cs)
  --   writeFile "_outreal" out
  --   writeFile "_err"     err
  --   writeFile "_outexp"  (correrSimulacion cs)

  Q.assert $ err == "" && out == correrSimulacion cs
--------------------------------------------------------------------------------

main = do
  -- Cola --
  "Compile Cola" ~::
    compile "Cola" ""
  "Run Cola" ~!::
    quickCheckWith stdArgs { maxSuccess = 100 } prop_cola
  removeFile (bin "Cola" "")

  -- ColaEnlazada --
  "Compile ColaEnlazada" ~::
    compile "Cola" "Enlazada"
  "Run ColaEnlazada" ~!::
    quickCheckWith stdArgs { maxSuccess = 100 } prop_cola_enlazada
  -- removeFile (bin "Cola" "Enlazada")

  -- Teoría de Conjuntos --
  "Compile TeoriaConjuntos" ~::
    compile "TeoriaConjuntos" ""
  "Run TeoriaConjuntos" ~::
    readProcessWithExitCode (bin "TeoriaConjuntos" "") [] "" >>=
    assertEqual "" (ExitSuccess, "", "")
  removeFile (bin "TeoriaConjuntos" "")
