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
  , blockCount  :: Word                        -- Cantidad de bloques básicos en el programa
  , instrs      :: DS.Seq (Named Instruction)  -- Lista de instrucciones en el bloque básico actual
  , bblocs      :: DS.Seq (BasicBlock)         -- Lista de bloques básicos en la definición actual
  , moduleDefs  :: DS.Seq (Definition)
  } deriving (Show)


newtype LLVM a = LLVM { unLLVM :: State CodegenSt a }
  deriving (Functor, Applicative, Monad, MonadState CodegenSt)


emptyCodegen :: CodegenSt
emptyCodegen = CodeGenSt 0 0 DS.empty DS.empty DS.empty


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
  modify $ \s -> s { bblocs = DS.empty }
  modify $ \s -> s { moduleDefs = defs DS.|> defineProc name params (toList bbl) }


addBasicBlock :: Name -> Named Terminator -> LLVM ()
addBasicBlock name t800 = do
  lins <- gets instrs
  bbl  <- gets bblocs
  modify $ \s -> s { instrs = DS.empty }
  modify $ \s -> s { bblocs = bbl DS.|> BasicBlock name (toList lins) t800 }


addNamedInstruction :: Type -> String -> Instruction -> LLVM (Operand)
addNamedInstruction t name ins = do
    lins <- gets instrs
    let r = Name name
    modify $ \s -> s { instrs = lins DS.|> (r := ins) }
    return $ local t r


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


--Falta arreglo
createInstruction :: MyAST.AST T.Type -> LLVM ()
createInstruction (MyAST.LAssign (((id, t), _):_) (e:_) _ _) = do
    e' <- createExpression e
    let (t', r) = (toType t, TE.unpack id)
    i <- alloca t' r
    store t' i e'
    return ()


createInstruction (MyAST.Write True e _ t) = do
    e' <- createExpression e
    addUnNamedInstruction (toType t) $ Call False CC.C [] (Right (definedFunction i32 (Name "writeLnInt"))) [(e', [])] [] []
    return ()


createInstruction (MyAST.Block _ _ accs _) = do
    mapM_ createInstruction accs
    return ()


createInstruction (MyAST.Convertion tType _ exp t) = do
    let t' = MyAST.tag exp 
    exp' <- createExpression exp
    addUnNamedInstruction (toType t) $ irConvertion tType t' exp'
    return ()


definedFunction :: Type -> Name -> Operand
definedFunction ty = ConstantOperand . (C.GlobalReference ty)


alloca :: Type -> String -> LLVM Operand
alloca ty r =
    addNamedInstruction ty r $ Alloca ty Nothing 0 []


store :: Type -> Operand -> Operand -> LLVM Operand
store t ptr val =
    addUnNamedInstruction t $ Store False ptr val Nothing 0 []


createExpression :: MyAST.AST T.Type -> LLVM (Operand)
createExpression (MyAST.ID _ id t) = do
    let (r, ty) = (TE.unpack id, toType t)
    load (TE.unpack id) ty


createExpression (MyAST.Int _ n _) = do
    return $ ConstantOperand $ C.Int 32 n


createExpression (MyAST.Float _ n _) = do
    return $ ConstantOperand $ C.Float $ Double n


createExpression (MyAST.Bool _ True  _) = do
   return $ ConstantOperand $ C.Int 8 1 
 

createExpression (MyAST.Bool _ False _) = do
   return $ ConstantOperand $ C.Int 8 0 
 

createExpression (MyAST.Char _ n _) = do
    return $ ConstantOperand $ C.Int 8 $ toInteger $ digitToInt n


createExpression (MyAST.Arithmetic op _ lexp rexp t) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    addUnNamedInstruction (toType t) $ irArithmetic op t lexp' rexp'
 
 
createExpression (MyAST.Boolean op _ lexp rexp t) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    addUnNamedInstruction (toType t) $ irBoolean op lexp' rexp'
 
 
createExpression (MyAST.Relational op _ lexp rexp t) = do
    lexp' <- createExpression lexp
    rexp' <- createExpression rexp
    addUnNamedInstruction (toType t) $ irRelational op lexp' rexp'




load :: String -> Type -> LLVM (Operand)
load name ty = do 
  addUnNamedInstruction ty $ Load False (local ty (Name name)) Nothing 0 []


createBasicBlocks :: [MyAST.AST T.Type] -> Named Terminator -> LLVM ()
createBasicBlocks accs m800 = do
  n <- getCount
  let r = UnName n
  mapM_ createInstruction accs
  addBasicBlock r m800


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
toType T.MyBool  = i8
toType T.MyChar  = i8


local :: Type -> Name -> Operand
local = LocalReference


retVoid :: LLVM (Named Terminator)
retVoid = do 
  n <- getCount
  return $ (UnName n) := Ret Nothing []


-- astNodeToLLVM :: MyAST.AST T.Type -> LLVM ()   
-- astNodeToLLVM (MyAST.DefProc name accs _ _ _ _) = do
--   defineProc (TE.unpack name) [] (instListToBasicBlocks accs)
-- 
-- instListToBasicBlocks :: [MyAST.AST T.Type] -> [BasicBlock]
-- 
-- createBlocks :: CodegenSt -> [BasicBlock]
-- createBlocks st = [BasicBlock (Name "program") (toList (instrs st)) ((UnName 100) := (Ret Nothing []))]
