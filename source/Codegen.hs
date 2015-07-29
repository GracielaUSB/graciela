{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import qualified LLVM.General.AST.Constant as C
import qualified Data.Sequence             as DS
import qualified Data.Text                 as TE
import qualified Type                      as T
import qualified AST                       as MyAST
import qualified Data.Map                  as M
import LLVM.General.AST                    as AST
import Data.List                           as L
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
import Data.Function

data CodegenSt
  = CodeGenSt {
    currentName :: Name
  , countBlock  :: Int
  , count       :: Word              -- Cantidad de instrucciones sin nombre
  , blocks      :: M.Map Name BlockSt -- Lista de instrucciones del programa
  } deriving (Show)


data BlockSt
  = BlockSt {
    idx     :: Int              
  , instrs  :: DS.Seq (Named Instruction)  -- Lista de instrucciones del bloque
  , term    :: Maybe  (Named Terminator) 
  } deriving (Show)


newtype Codegen a = Codegen { runCodegen :: State CodegenSt a }
  deriving (Functor, Applicative, Monad, MonadState CodegenSt)


newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module)


runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)


createBlocks :: CodegenSt -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ M.toList (blocks m)

--createBlocks :: CodegenSt -> [BasicBlock]
--createBlocks st = [BasicBlock (Name "program") (toList (instrs st)) ((Name "final") := (Ret Nothing []))]
  

makeBlock :: (Name, BlockSt) -> BasicBlock
makeBlock (l, (BlockSt _ s t)) = BasicBlock l (toList s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)


sortBlocks :: [(Name, BlockSt)] -> [(Name, BlockSt)]
sortBlocks = L.sortBy (compare `on` (idx . snd))


addBlock :: String -> Codegen Name
addBlock bname = do
  blks <- gets blocks
  i    <- gets countBlock
  let new  = emptyBlock i
      name = Name (bname ++ show i)
  modify $ \s -> s { blocks = M.insert name new blks, countBlock = i + 1}
  return name


setBlock :: Name -> Codegen Name
setBlock name = do
  modify $ \s -> s { currentName = name }
  return name


modifyBlock :: BlockSt -> Codegen ()
modifyBlock new = do
  act <- gets currentName
  modify $ \s -> s { blocks = M.insert act new (blocks s) }


emptyBlock :: Int -> BlockSt
emptyBlock i = BlockSt i DS.empty Nothing



emptyCodegen :: CodegenSt
emptyCodegen = CodeGenSt (Name "") 0 0 M.empty


execCodegen :: Codegen a -> CodegenSt
execCodegen m = execState (runCodegen m) emptyCodegen


local :: Type -> Name -> Operand
local = LocalReference


global :: Type -> Name -> C.Constant
global = C.GlobalReference 


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


defineFunc ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
defineFunc retTy label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retTy
  , basicBlocks = body
  }


current :: Codegen BlockSt
current = do
  name <- gets currentName
  blks <- gets blocks
  case M.lookup name blks of
  { Just x  -> return x
  ; Nothing -> error $ "Error Bloque: " ++ show name
  }


terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  block <- current
  modifyBlock (block { term = Just trm })
  return trm


getCount :: Codegen Word
getCount = do
  n <- gets count
  modify $ \s -> s { count = n + 1 }
  return $ n + 1


instr :: Type -> Instruction -> Codegen (Operand)
instr t ins = do
  n <- getCount
  let ref = (UnName n)
  block <- current
  let xs = instrs block
  modifyBlock $ block { instrs = xs DS.|> (ref := ins) } 
  return $ local t ref


astToLLVM :: MyAST.AST T.Type -> LLVM ()
astToLLVM (MyAST.Program name _ _ (acc:_) _) = do
  defineProc (TE.unpack name) [] bls
  where
    bls = createBlocks $ execCodegen $ astToInstr acc




--astToLLVM :: MyAST.AST T.Type -> LLVM ()
--astToLLVM (MyAST.FunBody fname _ body bound _) = do
--  let name = (TE.unpack fname)
--  retTy <- Buscar Tipo de retorno en la tabla
--  args  <- Buscar argumentos
--  defineFunc retTy name fnArgs bls
--  where
--    fnArgs = toSig args
--    bls    = createBlocks $ execCodegen $ do 
--        entry <- addBlock name
--        setBlock name
--        forM args $ \a -> do
--          var <- alloca TYPEarg
--          store var (local (AST.Name a))
--          assign a var
--        astToInstr body >>= ret


-- Hay q cambiarla
toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

astToInstr :: MyAST.AST T.Type -> Codegen (Operand)
astToInstr (MyAST.Arithmetic op _ lexp rexp t) = do
  lexp' <- astToInstr lexp
  rexp' <- astToInstr rexp
  instr (toType t) $ irArithmetic op t lexp' rexp'


astToInstr (MyAST.Boolean op _ lexp rexp t) = do
  lexp' <- astToInstr lexp
  rexp' <- astToInstr rexp
  instr (toType t) $ irBoolean op lexp' rexp'


astToInstr (MyAST.Relational op _ lexp rexp t) = do
  lexp' <- astToInstr lexp
  rexp' <- astToInstr rexp
  instr (toType t) $ irRelational op lexp' rexp'


astToInstr (MyAST.Convertion tType _ exp t) = do
  let t' = MyAST.tag exp 
  exp' <- astToInstr exp
  instr (toType t) $ irConvertion tType t' exp'


astToInstr (MyAST.LAssign (((id, t), _):_) (e:_) _ _) = do
  e' <- astToInstr e
  let (t', r) = (toType t, (Name (TE.unpack id)))
  i <- alloca t' r
  store t' i e'
  return i


astToInstr (MyAST.Block _ _ (a:_) _) = do
  astToInstr a


astToInstr (MyAST.Int _ n _) = do
  return $ ConstantOperand $ C.Int 32 n

astToInstr (MyAST.Float _ n _) = do
  return $ ConstantOperand $ C.Float $ Double n

astToInstr (MyAST.Bool _ True  _) = do
  return $ ConstantOperand $ C.Int 8 1 

astToInstr (MyAST.Bool _ False _) = do
  return $ ConstantOperand $ C.Int 8 0 

astToInstr (MyAST.Char _ n _) = do
  return $ ConstantOperand $ C.Int 8 $ toInteger $ digitToInt n



alloca :: Type -> Name -> Codegen Operand
alloca ty r = instr ty $ Alloca ty Nothing 0 []  

store :: Type -> Operand -> Operand -> Codegen Operand
store t ptr val = instr t $ Store False ptr val Nothing 0 []


toType :: T.Type -> Type
toType T.MyInt   = i32
toType T.MyFloat = double
toType T.MyBool  = i8
toType T.MyChar  = i8

