{-# LANGUAGE NamedFieldPuns #-}

module LLVM.Program where

--------------------------------------------------------------------------------
import           Aborts
import           AST.Definition
import           AST.Instruction                         (Instruction)
import           AST.Program
import           LLVM.Type                               (intType)
-- import           Limits
import           LLVM.State
import           LLVM.Definition                         (preDefinitions,
                                                          definition,
                                                          mainDefinition)
-- import           LLVM.Expression
-- import           LLVM.Instruction
-- import           LLVM.Quantification
-- import           SymbolTable
-- import qualified Type                                    as T
--------------------------------------------------------------------------------
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad.State                     (void, evalState)
import           Data.Foldable                           (toList)
import qualified Data.Map                                as DM
import           Data.Maybe
import           Data.Monoid                             ((<>))
import           Data.Range.Range                        as RA
import qualified Data.Text                               as T
import           Data.Word
import           LLVM.General.AST                        (Definition (..),
                                                          Module (..),
                                                          Parameter (..),
                                                          defaultModule)
import           LLVM.General.AST.Global                 (Global(..),functionDefaults)
import           LLVM.General.AST.Attribute
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.FloatingPointPredicate as FL
import           LLVM.General.AST.Instruction            (FastMathFlags (..),
                                                          Instruction,
                                                          Named (..),
                                                          Terminator (..))
import qualified LLVM.General.AST.IntegerPredicate       as IL
import           LLVM.General.AST.Name                   (Name (..))
import           LLVM.General.AST.Operand                (CallableOperand,
                                                          Operand (..))
import           LLVM.General.AST.Type
import           System.Info                             (arch, os)
import           System.Process                          (callCommand, readProcess)
--------------------------------------------------------------------------------




-- addFile :: String -> LLVM ()
-- addFile file = globalVariable (Name (convertFile file)) (ptr pointerType) (C.Null (ptr pointerType))


programToLLVM :: [String] -> Program -> IO Module
programToLLVM files (Program name _ defs insts) = do
  -- Eval the program with the LLVMState 
  let definitions = evalState (unLLVM program) initialState
  version <- getOSXVersion -- Mac OS only

  return defaultModule
    { moduleName         = T.unpack name
    , moduleDefinitions  = definitions
    , moduleTargetTriple = Just $ whichTarget version
    }
  where
    -- merge all predefined definitions (e.g read, write), user functions/procedures
    -- and the main program, that will be a function called main... of course.
    -- TODO add also all types and abstract types as Definition's `TypeDefinition`
    program = do 
      definitions <- mapM definition defs
      preDef <- preDefinitions files
      main   <- mainDefinition insts
      return $ preDef <> definitions <> [main]

    -- the Triple Target is a string that allow LLVM know the OS, fabricant and OS version
    whichTarget version = case os of
      "darwin"  -> arch <> "-apple-macosx" <> version -- With Mac, version needs to end with "0",
                                                           -- example: 11.10.3 -> 11.10.0
      "linux"   -> arch <> "-unknown-linux-gnu"
      "windows" -> undefined
    -- As mentioned above, Macs need a version ended with .0
    crop str = if last str == '.'
        then str <> "0"
        else crop $ init str
    -- Gets OSX version
    getOSXVersion :: IO String
    getOSXVersion = case os of
      "darwin" -> do
        crop <$> (readProcess "/usr/bin/sw_vers" ["-productVersion"] [])
      _        -> return ""
    


-- openFile :: String -> LLVM Operand
-- openFile file = do
--   let file' = convertFile file
--   ops <- addFileNameOpe file
--   op  <- caller (ptr charType) (Right $ definedFunction (ptr charType) (Name openFileStr)) [(ops, [])]
--   store (ptr charType) (ConstantOperand $ C.GlobalReference (ptr charType) (Name file')) op


-- closeFile :: String -> LLVM Operand
-- closeFile file = do
--   let file' = convertFile file
--   -- Cargamos la variable gobal perteneciente al archivo.
--   let load' = Load False (ConstantOperand $ C.GlobalReference (ptr charType) (Name file')) Nothing 0 []
--   op <- addUnNamedInstruction voidType load'
--   caller voidType (Right $ definedFunction voidType (Name closeFileStr)) [(op, [])]


-- createLLVM :: [String] -> [Definition] -> Instruction -> LLVM Module
-- createLLVM files defs accs = do
--   createPreDef files
--   mapM createDef defs
--   m800 <- retVoid
--   mapM_ openFile files
--   createInstruction accs
--   mapM_ closeFile files
--   addBasicBlock m800
--   addDefinition "main" ([],False) voidType
--   return ()


-- addArgOperand :: [(String, Contents SymbolTable)] -> LLVM ()
-- addArgOperand [] = return ()

-- addArgOperand ((id',c):xs) = do
--   let t    = toType $ argType c
--   let tp   = argTypeArg c
--   let id   = convertId id'
--   let e'   = local t (Name id')

--   case tp of
--     T.InOut -> do exp <- addUnNamedInstruction t $ Load False e' Nothing 0 []
--                   op  <- alloca Nothing t id
--                   store t op exp
--                   addVarOperand id' op

--     T.In    -> do op <- alloca Nothing t id
--                   store t op e'
--                   addVarOperand id' op

--     T.Out   -> do op <- alloca Nothing t id
--                   addVarOperand id' op
--                   initialize id $ argType c
--                   return ()

--     T.Ref   -> addVarOperand id' e'

--   addArgOperand xs


-- retVarOperand :: [(String, Contents SymbolTable)] -> LLVM ()
-- retVarOperand [] = pure ()

-- retVarOperand ((id', c):xs) = do

--   let t   = toType $ getContentType c
--   let exp = local t (Name id')
--   let tp  = argTypeArg c

--   case tp of
--     T.InOut -> do add <- load id' t
--                   store t exp add
--                   return ()

--     T.Out   -> do add <- load id' t
--                   store t exp add
--                   return ()

--     T.In     -> return ()
--     T.Ref    -> return ()

--   retVarOperand xs


-- addFuncParam :: (TE.Text, T.Type) -> LLVM ()
-- addFuncParam (id'', t@(T.GArray _ _)) = do
--   let id = TE.unpack id''
--   let e'   = local (toType t) (Name id)
--   addVarOperand id e'
--   return ()
-- addFuncParam _ = return ()






-- createBasicBlocks :: [AST] -> Named Terminator -> LLVM ()
-- createBasicBlocks accs m800 = genIntructions accs
--   where
--     genIntructions (acc:xs) = do
--       createInstruction acc
--       genIntructions xs
--     genIntructions [] =
--       addBasicBlock m800
