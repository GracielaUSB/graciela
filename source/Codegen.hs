{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import qualified LLVM.General.AST.Constant as C
import qualified Data.Sequence             as DS
import qualified Data.Text                 as TE
import qualified Type                      as T
import qualified AST                       as MyAST
import qualified LLVM.General.AST.CallingConvention as CC
import LLVM.General.AST                    as AST
import LLVM.General.AST.Global
import LLVM.General.AST.Float
import LLVM.General.AST.Type 
import Control.Monad.State
import Control.Applicative
import LLVM.General.Module
import Data.Foldable (toList)
import Data.Word
import Data.Char
import IR
import SymbolTable
import qualified Data.Map as DM
import Contents
import Data.Maybe

-- emptyModule :: String -> AST.Module
-- emptyModule label = defaultModule { moduleName = label }
-- 
-- 
-- addDefn :: Definition -> LLVM ()
-- addDefn d = do
--   defs <- gets moduleDefinitions
--   modify $ \s -> s { moduleDefinitions = defs ++ [d] }

--runLLVM :: AST.Module -> LLVM a -> AST.Module
--runLLVM = flip (execState . unLLVM)
--
-- newtype Codegen a = Codegen { runCodegen :: State CodegenSt a }
--   deriving (Functor, Applicative, Monad, MonadState CodegenSt)
--
-- runLLVM (emptyModule name) $ createLLVM defs accs
--
-- astToLLVM :: MyAST.AST T.Type -> LLVM ()
-- astToLLVM (MyAST.Program name _ defs accs _) = do
--   mapM_ defineProc (TE.unpack name) [] defs' accs'
--   where
--     bls = createBlocks $ execCodegen $ astToInstr acc

data CodegenSt
  = CodeGenSt {
    insCount    :: Word                        -- Cantidad de instrucciones sin nombre
  , blockName   :: Name                        -- Cantidad de bloques básicos en el programa
  , instrs      :: DS.Seq (Named Instruction)  -- Lista de instrucciones en el bloque básico actual
  , bblocs      :: DS.Seq BasicBlock         -- Lista de bloques básicos en la definición actual
  , moduleDefs  :: DS.Seq Definition
  , varsLoc     :: DM.Map String Operand
  } deriving (Show)

newtype LLVM a = LLVM { unLLVM :: State CodegenSt a }
  deriving (Functor, Applicative, Monad, MonadState CodegenSt)

emptyCodegen :: CodegenSt
emptyCodegen = CodeGenSt 1 (UnName 0) DS.empty DS.empty DS.empty DM.empty

execCodegen :: LLVM a -> CodegenSt
execCodegen m = execState (unLLVM m) emptyCodegen

astToLLVM :: MyAST.AST T.Type -> AST.Module
astToLLVM (MyAST.Program name _ defs accs _) =
    defaultModule { moduleName        = TE.unpack name
                  , moduleDefinitions = toList $ moduleDefs $ execCodegen $ createLLVM defs accs
    }

createLLVM :: [MyAST.AST T.Type] -> [MyAST.AST T.Type] -> LLVM ()
createLLVM defs accs = do
  addDefinition "writeLnInt" [(i32, (Name ""))]
  mapM_ createDef defs
  m800 <- retVoid
  createBasicBlocks accs m800
  addDefinition "main" []


createDef :: MyAST.AST T.Type -> LLVM()
createDef (MyAST.DefProc name _ accs _ _ _ _ _) = do
    m800 <- retVoid
    createBasicBlocks accs m800
    addDefinition (TE.unpack name) []

addDefinition :: String -> [(Type, Name)] -> LLVM ()
addDefinition name params = do
  bbl  <- gets bblocs
  defs <- gets moduleDefs 
  modify $ \s -> s { bblocs  = DS.empty }
  modify $ \s -> s { varsLoc = DM.empty }
  modify $ \s -> s { moduleDefs = defs DS.|> defineProc name params (toList bbl) }

setLabel :: Name -> Named Terminator -> LLVM()
setLabel name t800 = do
    addBasicBlock t800
    modify $ \s -> s { blockName = name }

addBasicBlock :: Named Terminator -> LLVM ()
addBasicBlock t800 = do
  lins <- gets instrs
  bbl  <- gets bblocs
  name <- gets blockName
  modify $ \s -> s { instrs     = DS.empty }
  modify $ \s -> s { bblocs     = bbl DS.|> BasicBlock name (toList lins) t800 }

addNamedInstruction :: Type -> String -> Instruction -> LLVM (Operand)
addNamedInstruction t name ins = do
    lins <- gets instrs
    let r = Name name
    modify $ \s -> s { instrs = lins DS.|> (r := ins) }
    let op = local t r
    addVarOperand name op
    return op 

addVarOperand :: String -> Operand -> LLVM()
addVarOperand name op = do
    map <- gets varsLoc
    modify $ \s -> s { varsLoc = DM.insert name op map }

addUnNamedInstruction :: Type -> Instruction -> LLVM (Operand)
addUnNamedInstruction t ins = do
    n <- getCount
    lins <- gets instrs
    let r = UnName n
    modify $ \s -> s { instrs = lins DS.|> (r := ins) }
    return $ local t r

getCount :: LLVM Word
getCount = do
    n <- gets insCount
    modify $ \s -> s { insCount = n + 1 }
    return $ n

newLabel :: LLVM(Name)
newLabel = do
    n <- getCount
    let r = UnName $ n
    return r

sTableToAlloca :: SymbolTable -> LLVM ()
sTableToAlloca st = 
    mapM_ (uncurry alloca) $ map (\(id, c) -> ((toType . symbolType) c, TE.unpack id)) $ DM.toList $ (getMap . getActual) st
    
createInstruction :: MyAST.AST T.Type -> LLVM ()
createInstruction (MyAST.LAssign (((id, t), _):_) (e:_) _ _) = do
    e' <- createExpression e
    map <- gets varsLoc
    let (t', i) = (toType t, fromJust $ DM.lookup (TE.unpack id) map)
    store t' i e'
    return ()


createInstruction (MyAST.Write True e _ t) = do
    e' <- createExpression e
    addUnNamedInstruction (toType t) $ Call False CC.C [] (Right (definedFunction i32 (Name "writeLnInt"))) [(e', [])] [] []
    return ()

createInstruction (MyAST.Skip _ _) = return ()
 
createInstruction (MyAST.Block _ st _ accs _) = do
    sTableToAlloca st
    mapM_ createInstruction accs

createInstruction (MyAST.Cond guards _ _) = do
    final <- newLabel
    genGuards guards final final

createInstruction (MyAST.Rept guards _ _ _ _) = do
    final   <- newLabel
    initial <- newLabel
    setLabel initial $ branch initial
    genGuards guards final initial

branch label = Do $ Br label [] 

cond op true false = Do $ CondBr op true false []

genGuards (guard:[]) none one = do
    genGuard guard none
    setLabel none $ branch one

genGuards (guard:xs) none one = do
    next <- newLabel
    genGuard guard next
    setLabel next $ branch one
    genGuards xs none one

genGuard (MyAST.Guard guard acc _ _) next = do
    tag  <- createExpression guard
    code <- newLabel
    setLabel code $ cond tag code next
    createInstruction acc

definedFunction :: Type -> Name -> Operand
definedFunction ty = ConstantOperand . (C.GlobalReference ty)

alloca :: Type -> String -> LLVM Operand
alloca ty r = do
    addNamedInstruction ty r $ Alloca ty Nothing 0 []

store :: Type -> Operand -> Operand -> LLVM Operand
store t ptr val =
    addUnNamedInstruction t $ Store False ptr val Nothing 0 []

createExpression :: MyAST.AST T.Type -> LLVM (Operand)
createExpression (MyAST.Int _ n _) = do
    return $ ConstantOperand $ C.Int 32 n

createExpression (MyAST.Relational (MyAST.Less) _ lexp rexp _) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    addUnNamedInstruction bool $ irRelational MyAST.Less lexp' rexp'

createExpression (MyAST.ID _ id t) = do
  let (r, ty) = (TE.unpack id, toType t)
  load (TE.unpack id) ty

createExpression (MyAST.Arithmetic op _ lexp rexp t) = do
  lexp' <- createExpression lexp
  rexp' <- createExpression rexp
  addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'

bool = i8

load :: String -> Type -> LLVM (Operand)
load name ty = do 
  addUnNamedInstruction ty $ Load False (local ty (Name name)) Nothing 0 []

createBasicBlocks :: [MyAST.AST T.Type] -> Named Terminator -> LLVM ()
createBasicBlocks accs m800 = do
  genIntructions accs
    where
      genIntructions (acc:xs) = do
          r <- newLabel
          createInstruction acc
          genIntructions xs
      genIntructions [] = do
          r <- newLabel
          addBasicBlock m800

defineProc :: String -> [(Type, Name)] -> [BasicBlock] -> Definition
defineProc label argtys body =
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = VoidType
  , basicBlocks = body
  }

toType :: T.Type -> Type
toType T.MyInt   = i32
toType T.MyFloat = double
toType T.MyBool  = i1
toType T.MyChar  = i8

local :: Type -> Name -> Operand
local = LocalReference

retVoid :: LLVM (Named Terminator)
retVoid = do 
  n <- getCount
  return $ (UnName n) := Ret Nothing []
-- 
-- 
-- instr :: Type -> Instruction -> Codegen (Operand)
-- instr t ins = do
--   xs <- gets instrs
--   n  <- getCount
--   let ref = UnName n
--   modify $ \s -> s { instrs = xs DS.|> (ref := ins) }
--   return $ local t ref
--    
-- astNodeToLLVM :: MyAST.AST T.Type -> LLVM ()   
-- astNodeToLLVM (MyAST.DefProc name accs _ _ _ _) = do
--   defineProc (TE.unpack name) [] (instListToBasicBlocks accs)
-- 
-- instListToBasicBlocks :: [MyAST.AST T.Type] -> [BasicBlock]
-- 
-- createBlocks :: CodegenSt -> [BasicBlock]
-- createBlocks st = [BasicBlock (Name "program") (toList (instrs st)) ((UnName 100) := (Ret Nothing []))]
--   

-- astToInstr :: MyAST.AST T.Type -> Codegen (Operand)
-- astToInstr (MyAST.Arithmetic op _ lexp rexp t) = do
--   lexp' <- astToInstr lexp
--   rexp' <- astToInstr rexp
--   instr (toType t) $ irArithmetic op t lexp' rexp'
-- 
-- 
-- astToInstr (MyAST.Boolean op _ lexp rexp t) = do
--   lexp' <- astToInstr lexp
--   rexp' <- astToInstr rexp
--   instr (toType t) $ irBoolean op lexp' rexp'
-- 
-- 
-- astToInstr (MyAST.Relational op _ lexp rexp t) = do
--   lexp' <- astToInstr lexp
--   rexp' <- astToInstr rexp
--   instr (toType t) $ irRelational op lexp' rexp'
-- 
-- 
-- astToInstr (MyAST.Convertion tType _ exp t) = do
--   let t' = MyAST.tag exp 
--   exp' <- astToInstr exp
--   instr (toType t) $ irConvertion tType t' exp'
-- 
-- 
-- astToInstr (MyAST.LAssign (((id, t), _):_) (e:_) _ _) = do
--   e' <- astToInstr e
--   let (t', r) = (toType t, (Name (TE.unpack id)))
--   i <- alloca t' r
--   store t' i e'
--   return i
-- 
-- 
-- astToInstr (MyAST.Block _ _ xs _) = do
--   l <- mapM astToInstr xs
-- 
-- 
-- astToInstr (MyAST.Int _ n _) = do
--   return $ ConstantOperand $ C.Int 32 n
-- 
-- astToInstr (MyAST.Float _ n _) = do
--   return $ ConstantOperand $ C.Float $ Double n
-- 
-- astToInstr (MyAST.Bool _ True  _) = do
--   return $ ConstantOperand $ C.Int 8 1 
-- 
-- astToInstr (MyAST.Bool _ False _) = do
--   return $ ConstantOperand $ C.Int 8 0 
-- 
-- astToInstr (MyAST.Char _ n _) = do
--   return $ ConstantOperand $ C.Int 8 $ toInteger $ digitToInt n
-- 
-- 
-- 
-- 
