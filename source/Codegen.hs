{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Control.Monad.State
import Control.Applicative
import qualified LLVM.General.AST.Constant as C
import qualified Data.Sequence            as DS
import qualified AST as AST
import qualified Type as T
import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.Type 
import LLVM.General.AST.Float
import Data.Word
import Data.Char
import IR

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

  
astToInstr :: AST.AST T.Type -> Codegen (Operand)
astToInstr (AST.Arithmetic op _ lexp rexp t) = do
  lexp' <- astToInstr lexp
  rexp' <- astToInstr rexp
  instr (toType t) $ irArithmetic op t lexp' rexp'


astToInstr (AST.Boolean op _ lexp rexp t) = do
  lexp' <- astToInstr lexp
  rexp' <- astToInstr rexp
  instr (toType t) $ irBoolean op lexp' rexp'


astToInstr (AST.Relational op _ lexp rexp t) = do
  lexp' <- astToInstr lexp
  rexp' <- astToInstr rexp
  instr (toType t) $ irRelational op lexp' rexp'


astToInstr (AST.Convertion tType _ exp t) = do
  let t' = AST.tag exp 
  exp' <- astToInstr exp
  instr (toType t) $ irConvertion tType t' exp'


astToInstr (AST.Int _ n _) = do
  return $ ConstantOperand $ C.Int 32 n

astToInstr (AST.Float _ n _) = do
  return $ ConstantOperand $ C.Float $ Double n

astToInstr (AST.Bool _ True  _) = do
  return $ ConstantOperand $ C.Int 8 1 

astToInstr (AST.Bool _ False _) = do
  return $ ConstantOperand $ C.Int 8 0 

astToInstr (AST.Char _ n _) = do
  return $ ConstantOperand $ C.Int 8 $ toInteger $ digitToInt n


toType :: T.Type -> Type
toType T.MyInt   = i32
toType T.MyFloat = double
toType T.MyBool  = i8
toType T.MyChar  = i8

