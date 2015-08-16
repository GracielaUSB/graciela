{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
import qualified Data.Sequence                      as DS
import qualified Data.Text                          as TE
import qualified Data.Map                           as DM
import qualified Type                               as T
import qualified AST                                as MyAST
import LLVM.General.AST                             as AST
import LLVM.General.AST.Attribute
import LLVM.General.AST.Global
import LLVM.General.AST.Float
import LLVM.General.AST.Type 
import Control.Monad.State
import Control.Applicative
import LLVM.General.Module
import Data.Foldable (toList)
import SymbolTable
import Data.Maybe
import Data.Word
import Data.Char
import Contents
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
    addDefinitionProc "writeLnInt" [(Name "", i32)]
    mapM_ createDef defs
    m800 <- retVoid
    createBasicBlocks accs m800
    addDefinitionProc "main" []


createDef :: MyAST.AST T.Type -> LLVM()
createDef (MyAST.DefProc name _ accs _ _ _ _ _) = do
    m800 <- retVoid
    createBasicBlocks accs m800
    addDefinitionProc (TE.unpack name) []



--BOUNDDD
createDef (MyAST.DefFun fname st _ (MyAST.FunBody _ exp _) reType bound _) = 
    let funcCont  = DM.toList $ getMap $ getActual st
        args  = map (\(n, t) -> (Name $ TE.unpack n, toType $ symbolType t)) funcCont
    in do exp'  <- createExpression exp
          retTy <- retType exp'
          addBasicBlock retTy
          addDefinitionFunc (TE.unpack fname) args (toType reType)
          return ()


addDefinitionProc :: String -> [(Name, Type)] -> LLVM ()
addDefinitionProc name params = do
    bbl  <- gets bblocs
    defs <- gets moduleDefs 
    modify $ \s -> s { bblocs  = DS.empty }
    modify $ \s -> s { varsLoc = DM.empty }
    modify $ \s -> s { moduleDefs = defs DS.|> defineProc name params (toList bbl) }


addDefinitionFunc :: String -> [(Name, Type)] -> Type -> LLVM ()
addDefinitionFunc name params retTy = do
    bbl  <- gets bblocs
    defs <- gets moduleDefs 
    modify $ \s -> s { bblocs  = DS.empty }
    modify $ \s -> s { varsLoc = DM.empty }
    modify $ \s -> s { moduleDefs = defs DS.|> defineFunc name params retTy (toList bbl) }


newLabel :: LLVM (Name)
newLabel = do
    n <- getCount
    return $ UnName n


setLabel :: Name -> Named Terminator -> LLVM()
setLabel name t800 = do
    addBasicBlock t800
    modify $ \s -> s { blockName = name }


addBasicBlock :: Named Terminator -> LLVM ()
addBasicBlock t800 = do
    lins  <- gets instrs
    bbl   <- gets bblocs
    name  <- gets blockName
    name' <- newLabel
    modify $ \s -> s { blockName  = name'    }
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
    r    <- newLabel
    lins <- gets instrs
    modify $ \s -> s { instrs = lins DS.|> (r := ins) }
    return $ local t r


getCount :: LLVM Word
getCount = do
    n <- gets insCount
    modify $ \s -> s { insCount = n + 1 }
    return $ n


sTableToAlloca :: SymbolTable -> LLVM ()
sTableToAlloca st = 
    mapM_ (uncurry alloca) $ map (\(id, c) -> ((toType . symbolType) c, TE.unpack id))
                                                 $ DM.toList $ (getMap . getActual) st
    

--Falta arreglo, y lista
createInstruction :: MyAST.AST T.Type -> LLVM ()
createInstruction (MyAST.LAssign (((id, t), _):_) (e:_) _ _) = do
    e' <- createExpression e
    map <- gets varsLoc
    let (t', i) = (toType t, fromJust $ DM.lookup (TE.unpack id) map)
    store t' i e'
    return ()


createInstruction (MyAST.Write True e _ t) = do
    e' <- createExpression e
    addUnNamedInstruction (toType t) $ Call False CC.C [] (Right 
                 (definedFunction i32 (Name "writeLnInt"))) [(e', [])] [] []
    return ()


createInstruction (MyAST.Skip _ _) = return ()
 

createInstruction (MyAST.Block _ st _ accs _) = do
    sTableToAlloca st
    mapM_ createInstruction accs
    return ()


createInstruction (MyAST.Convertion tType _ exp t) = do
    let t' = MyAST.tag exp 
    exp' <- createExpression exp
    addUnNamedInstruction (toType t) $ irConvertion tType t' exp'
    return ()


createInstruction (MyAST.Cond guards _ _) = do
    final <- newLabel
    genGuards guards final final
    return ()


createInstruction (MyAST.Rept guards _ _ _ _) = do
    final   <- newLabel
    initial <- newLabel
    setLabel initial $ branch initial
    genGuards guards final initial


branch :: Name -> Named Terminator
branch label = Do $ Br label [] 


condBranch :: Operand -> Name -> Name -> Named Terminator
condBranch op true false = Do $ CondBr op true false []


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
    setLabel code $ condBranch tag code next
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
createExpression (MyAST.ID _ id t) = do
    let (r, ty) = (TE.unpack id, toType t)
    val <- load (TE.unpack id) ty
    return val

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
    let t' = MyAST.tag lexp 
    addUnNamedInstruction (toType t) $ irRelational op t' lexp' rexp'

--POR HAcer
createExpression (MyAST.FCallExp fname st _ args _) = do
   return $ ConstantOperand $ C.Int 8 0 


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


defineProc :: String -> [(Name, Type)] -> [BasicBlock] -> Definition
defineProc label args body =
    GlobalDefinition $ functionDefaults {
      name        = Name label
    , parameters  = ([Parameter t n [] | (n, t) <- args], False)
    , returnType  = VoidType
    , basicBlocks = body
    }


defineFunc :: String -> [(Name, Type)] -> Type -> [BasicBlock] -> Definition
defineFunc label args retTy body =
    GlobalDefinition $ functionDefaults {
      name        = Name label
    , parameters  = ([Parameter t n [] | (n, t) <- args], False)
    , returnType  = retTy
    , basicBlocks = body
    }

toType :: T.Type -> Type
toType T.MyInt   = i32
toType T.MyFloat = double
toType T.MyBool  = i1
toType T.MyChar  = i8


local :: Type -> Name -> Operand
local = LocalReference


retType :: Operand -> LLVM (Named Terminator)
retType op = do 
    n <- newLabel
    return $ n := Ret (Just op) []


retVoid :: LLVM (Named Terminator)
retVoid = do 
    n <- newLabel
    return $ n := Ret Nothing []
