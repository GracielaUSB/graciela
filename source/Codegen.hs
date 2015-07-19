{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Control.Monad.State
import Control.Applicative
import qualified Data.Sequence            as DS
import LLVM.General.AST
import LLVM.General.AST.Global
import qualified AST as MyAST
import LLVM.General.AST 
import IR
import qualified Type as T
import Data.Word
import qualified LLVM.General.AST.Constant as C
import LLVM.General.Module
import qualified LLVM.General.AST as AST
import qualified Data.Text as T
import Data.Foldable (toList)

data CodegenSt
  = CodeGenSt {
    count   :: Word              -- Cantidad de instrucciones sin nombre
  , instrs  :: DS.Seq (Named Instruction)  -- Lista de instrucciones del programa
  } deriving (Show)

newtype Codegen a = Codegen { runCodegen :: State CodegenSt a }
  deriving (Functor, Applicative, Monad, MonadState CodegenSt)

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

defineProc :: String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
defineProc label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = VoidType
  , basicBlocks = body
  }

emptyCodegen :: CodegenSt
emptyCodegen = CodeGenSt 0 DS.empty

execCodegen :: Codegen a -> CodegenSt
execCodegen m = execState (runCodegen m) emptyCodegen

local :: Type -> Name -> Operand
local = LocalReference

getCount :: Codegen Word
getCount = do
  n <- gets count
  modify $ \s -> s { count = n + 1 }
  return $ n + 1

instr :: Type -> Instruction -> Codegen (Operand)
instr t ins = do
  xs <- gets instrs
  n  <- getCount
  let ref = UnName n
  modify $ \s -> s { instrs = xs DS.|> (ref := ins) }
  return $ local t ref

astToLLVM :: MyAST.AST T.Type -> LLVM ()
astToLLVM (MyAST.Program name _ _ (acc:_) _) = do
  defineProc (T.unpack name) [] bls
  where
    bls = createBlocks $ execCodegen $ astToInstr acc

createBlocks :: CodegenSt -> [BasicBlock]
createBlocks st = [BasicBlock (Name "program") (toList (instrs st)) ((Name "final") := (Ret Nothing []))]
  
astToInstr :: MyAST.AST T.Type -> Codegen (Operand)
astToInstr (MyAST.Arithmetic op _ lexp rexp t) = do
  lexp' <- astToInstr lexp
  rexp' <- astToInstr rexp
  instr (toType t) $ irArithmetic op t lexp' rexp'

astToInstr (MyAST.Int _ n _) = do
  return $ ConstantOperand $ C.Int 32 n

astToInstr (MyAST.LAssign (((id, t), _):_) (e:_) _ _) = do
  e' <- astToInstr e
  let (t', r) = (toType t, (Name (T.unpack id)))
  i <- alloca t' r
  store t' i e'
  return i

astToInstr (MyAST.Block _ _ (a:_) _) = do
  astToInstr a

alloca :: Type -> Name -> Codegen Operand
alloca ty r = do
  xs <- gets instrs
  modify $ \s -> s { instrs = xs DS.|> (r := (Alloca ty Nothing 0 [])) }
  return $ local ty r

store :: Type -> Operand -> Operand -> Codegen Operand
store t ptr val = instr t $ Store False ptr val Nothing 0 []

toType :: T.Type -> Type
toType T.MyInt = IntegerType 32
