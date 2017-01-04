module Test (main) where

import           Control.Monad    (forever, unless, void)
import           Data.Maybe       (isNothing)
import           Data.Semigroup   (Semigroup (..))
import           Main             (Options (..), compile, defaultOptions)
import           Prelude          hiding (readFile)
import           System.Directory (getCurrentDirectory, removeFile)
import           System.FilePath  ((-<.>))
import           System.IO        (IOMode (..), SeekMode (..), hClose,
                                   hGetContents, hSeek, withFile, readFile)
import           System.IO.Temp   (openBinaryTempFile, openTempFile,
                                   withTempFile)
import           System.Process   (CreateProcess (..), StdStream (..),
                                   createProcess, proc)
import           Test.HUnit

compileAndRun :: String -> Assertion
compileAndRun name' = do
  (path, handle) <- openBinaryTempFile "." (name -<.> ".bin")

  v1 <- compile name defaultOptions
    { optOutName      = Just path
    , optLibGraciela  = "libgraciela.so"
    , optLibGracielaA = "libgraciela-abstract.so" }

  unless (isNothing v1) (removeFile path)
  isNothing v1 @? "Compilation failed"

  hClose handle

  withFile (name -<.> ".in") ReadMode $ \hin -> do
    (_, Just hout, Just herr, _) <- createProcess (proc path [])
      { std_in  = UseHandle hin
      , std_out = CreatePipe
      , std_err = CreatePipe }

    removeFile path

    expectedout <- readFile (name -<.> ".expout")
    actualout   <- hGetContents hout

    actualout @?= expectedout

    expectederr <- readFile (name -<.> ".experr")
    actualerr   <- hGetContents herr

    actualerr @?= expectederr

  where
    name = "testset/" <> name' <> ".gcl"

tests = TestList
  [ "Cola" ~: compileAndRun "Cola" ]

main = do
  runTestTT tests
