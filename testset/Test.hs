module Test (main) where

import           Control.Monad    (forever, unless, void)
import           Data.Maybe       (isNothing)
import           Data.Semigroup   (Semigroup (..))
import           Main             (Options (..), defaultOptions)
import qualified Main             as M (compile)
import           Prelude          hiding (readFile)
import           System.Directory (getCurrentDirectory, removeFile)
import           System.FilePath  ((-<.>), (<.>), (</>))
import           System.IO        (IOMode (..), SeekMode (..), hClose,
                                   hGetContents, hSeek, readFile, withFile)
import           System.IO.Temp   (openBinaryTempFile, openTempFile,
                                   withTempFile)
import           System.Process   (CreateProcess (..), StdStream (..),
                                   createProcess, proc)
import           Test.HUnit

dir :: String
dir = "testset"

bin    name   = dir </> name </> name <.> "bin"
gcl    name   = dir </> name </> name <.> "gcl"
input  name n = dir </> name </> "in"  <> show n
output name n = dir </> name </> "out" <> show n
errput name n = dir </> name </> "err" <> show n

compile :: String -> Assertion
compile name = do
  v1 <- M.compile (gcl name) defaultOptions
    { optOutName      = Just (bin name)
    , optLibGraciela  = "libgraciela.so"
    , optLibGracielaA = "libgraciela-abstract.so" }

  unless (isNothing v1) (removeFile (bin name))
  isNothing v1 @? "Compilation failed"

run :: String -> Int -> Assertion
run name n = withFile (input name n) ReadMode $ \hin -> do
  (_, Just hout, Just herr, _) <- createProcess (proc (bin name) [])
    { std_in  = UseHandle hin
    , std_out = CreatePipe
    , std_err = CreatePipe }

  expectedout <- readFile (output name n)
  actualout   <- hGetContents hout
  expectederr <- readFile (errput name n)
  actualerr   <- hGetContents herr

  (actualout, actualerr) @?= (expectedout, expectederr)

tests = TestList
  [ "Cola" ~: TestList
    [ "Compile" ~: compile "Cola"
    , "Run"     ~: TestList $ TestCase . run "Cola" <$> [1..2]
    , "Delete"  ~: removeFile (bin "Cola") ]
  , "TeoriaConjuntos" ~: TestList
    [ "Compile" ~: compile "TeoriaConjuntos"
    , "Run"     ~: TestList $ TestCase . run "TeoriaConjuntos" <$> [1]
    , "Delete"  ~: removeFile (bin "TeoriaConjuntos") ]
  ]

main = do
  runTestTT tests
