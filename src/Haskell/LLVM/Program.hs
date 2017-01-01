{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module LLVM.Program where

--------------------------------------------------------------------------------
import           AST.Definition
import           AST.Instruction                         (Instruction)
import           AST.Program
import qualified AST.Program                             as P (structs)
import           AST.Type
import qualified AST.Type                                as G (Type)
import           Common
import           LLVM.Abort
import           LLVM.Definition                         (definition,
                                                          mainDefinition,
                                                          preDefinitions)
import           LLVM.Monad
import           LLVM.State
import qualified LLVM.State                              as S (structs)
import           LLVM.Struct                             (defineStruct)
import           LLVM.Type                               (intType)
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad.Trans.State.Strict        (evalState)
import           Data.Array                              (listArray)
import qualified Data.ByteString                         as BS (unpack)
import           Data.Foldable                           (toList)
import           Data.List                               (sortOn)
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map (keys, size,
                                                                 toAscList,
                                                                 toList)
import           Data.Sequence                           (fromList, singleton)
import           Data.Text                               (Text, unpack)
import           Data.Text.Encoding                      (encodeUtf8)
import           Data.Word
import           LLVM.General.AST                        (Definition (..),
                                                          Module (..),
                                                          Parameter (..),
                                                          defaultModule)
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.FloatingPointPredicate as FL
import qualified LLVM.General.AST.Global                 as G (Global (..),
                                                               functionDefaults,
                                                               globalVariableDefaults)
import           LLVM.General.AST.Instruction            (FastMathFlags (..),
                                                          Instruction,
                                                          Named (..),
                                                          Terminator (..))
import qualified LLVM.General.AST.IntegerPredicate       as IL
import           LLVM.General.AST.Operand                (Operand (..))
import           LLVM.General.AST.Type
import           System.Info                             (arch, os)
import           System.Process                          (readProcess)
--------------------------------------------------------------------------------

-- addFile :: String -> LLVM ()

programToLLVM :: [String]             -- ^ Files for read instructions
              -- -> Map Text (G.Type, a) -- ^ Declared types
              -> Program              -- ^ AST
              -> IO Module
programToLLVM
  files
  -- types
  Program { name, defs, insts, P.structs, fullStructs, strings }
  = do
  -- Eval the program with the LLVMRWS
  let definitions = evalState (unLLVM program) initialState
  version <- getOSXVersion -- Mac OS only

  return defaultModule
    { moduleName         = unpack name
    , moduleDefinitions  = toList definitions
    , moduleTargetTriple = Just $ whichTarget version
    }
  where
    -- merge all predefined definitions (e.g read, write), user functions/procedures
    -- and the main program, that will be a function called main... of course.
    -- TODO add also all types and abstract types as Definition's `TypeDefinition`

    program = do
      S.structs .= structs
      fullDataTypes .= fullStructs
      stringIds .= strings
      addStrings

      preDefinitions files
      mapM_ (uncurry defineStruct) . Map.toList $ fullStructs

      mapM_ definition defs
      mainDefinition insts files
      use moduleDefs


    -- the Triple Target is a string that allow LLVM know the OS, fabricant and OS version
    whichTarget version = case os of
      "darwin"  -> arch <> "-apple-macosx" <> version -- With Mac, version needs to end with "0",
                                                           -- example: 11.10.3 -> 11.10.0
      "linux"   -> arch <> "-pc-linux-gnu"
      "windows" -> undefined
    -- As mentioned above, Macs need a version ended with .0
    crop str = if last str == '.'
        then str <> "0"
        else crop $ init str
    -- Gets OSX version
    getOSXVersion :: IO String
    getOSXVersion = case os of
      "darwin" ->
        crop <$> readProcess "/usr/bin/sw_vers" ["-productVersion"] []
      _        -> return ""

    addStrings :: LLVM ()
    addStrings = do
      strs <- use stringIds
      ops <- mapM addString . sortOn snd . Map.toAscList $ strs
      stringOps .= listArray (0, Map.size strs - 1) ops

    addString :: (Text, Int) -> LLVM Operand
    addString (theString, i) = do
      let
        -- Convert the string into an array of 8-bit chars
        chars = BS.unpack . encodeUtf8 $ theString
      -- Get the length of the string
        n  = fromIntegral . succ . length $ chars
      -- Create an array type

      name <- newLabel "string"
      -- Create a global definition for the string
      addDefinition $ GlobalDefinition G.globalVariableDefaults
        { G.name        = name
        , G.isConstant  = True
        , G.type'       = ArrayType n i8
        , G.initializer = Just . C.Array i8 $
          [ C.Int 8 (toInteger c) | c <- chars ] <> [ C.Int 8 0 ]
        }
      pure . ConstantOperand $ C.GetElementPtr
        { C.inBounds = True
        , C.address = C.GlobalReference i8 name
        , C.indices = [C.Int 64 0, C.Int 64 0] }
