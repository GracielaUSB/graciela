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

data CodegenSt
  = CodeGenSt {
    count   :: Word              -- Cantidad de instrucciones sin nombre
  , instrs  :: DS.Seq Instruction  -- Lista de instrucciones del programa
  } deriving (Show)

newtype Codegen a = Codegen { runCodegen :: State CodegenSt a }
  deriving (Functor, Applicative, Monad, MonadState CodegenSt)

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
  modify $ \s -> s { instrs = xs DS.|> ins }
  return $ local t (UnName n)

  
astToInstr :: MyAST.AST T.Type -> Codegen (Operand)
astToInstr (MyAST.Arithmetic op _ lexp rexp t) = do
  lexp' <- astToInstr lexp
  rexp' <- astToInstr rexp
  instr (toType t) $ irArithmetic op t lexp' rexp'

astToInstr (MyAST.Int _ n _) = do
  return $ ConstantOperand $ C.Int 32 n

toType :: T.Type -> Type
toType T.MyInt = IntegerType 32
