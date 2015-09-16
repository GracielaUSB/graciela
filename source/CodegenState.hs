{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CodegenState where

import qualified Data.Map                                as DM
import qualified Data.Sequence                           as DS
import qualified AST                                     as MyAST
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified Data.Text                               as TE
import qualified LLVM.General.AST.Constant               as C
import qualified Location                                as L
import qualified Type                                    as T
import LLVM.General.AST                                  as AST
import LLVM.General.AST.Global                           as GLOB
import LLVM.General.AST.InlineAssembly
import LLVM.General.AST.Attribute
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Float
import LLVM.General.AST.Type 
import LLVM.General.Module
import Control.Monad.State
import Control.Applicative
import Data.Foldable (toList)
import SymbolTable
import Data.Maybe
import Data.Word
import Data.Char
import Contents


data CodegenSt
  = CodeGenSt {
    insCount    :: Word                        -- Cantidad de instrucciones sin nombre
  , blockName   :: Name                        -- Cantidad de bloques básicos en el programa
  , instrs      :: DS.Seq (Named Instruction)  -- Lista de instrucciones en el bloque básico actual
  , bblocs      :: DS.Seq BasicBlock           -- Lista de bloques básicos en la definición actual
  , moduleDefs  :: DS.Seq Definition
  , varsLoc     :: DM.Map String Operand
  , arrsDim     :: DM.Map String [Operand]
  , loc         :: L.Location
  } deriving (Show)


newtype LLVM a = LLVM { unLLVM :: State CodegenSt a }
  deriving (Functor, Applicative, Monad, MonadState CodegenSt)


emptyCodegen :: CodegenSt
emptyCodegen = CodeGenSt 1 (UnName 0) DS.empty DS.empty DS.empty DM.empty DM.empty L.emptyLoc


execCodegen :: LLVM a -> CodegenSt
execCodegen m = execState (unLLVM m) emptyCodegen


newLabel :: LLVM (Name)
newLabel = do
    n <- getCount
    return $ UnName n


addDimToArray :: String -> Operand -> LLVM()
addDimToArray name op = do
    dims <- gets arrsDim
    modify $ \s -> s { arrsDim = DM.insertWith (++) name [op] dims }


addDefinition :: String -> ([Parameter], Bool) -> Type -> LLVM ()
addDefinition name params retTy = do
    bbl  <- gets bblocs
    defs <- gets moduleDefs 
    let def = GlobalDefinition $ functionDefaults {
                name        = Name name
              , parameters  = params
              , returnType  = retTy
              , basicBlocks = (toList bbl)
              }
    modify $ \s -> s { bblocs  = DS.empty }
    modify $ \s -> s { varsLoc = DM.empty }
    modify $ \s -> s { moduleDefs = defs DS.|> def}


addString :: String -> Name -> Type -> LLVM ()
addString msg name ty = do
    defs <- gets moduleDefs
    let def = GlobalDefinition $ globalVariableDefaults {
                name        = name
              , isConstant  = True
              , GLOB.type'  = ty  
              , initializer = Just $ stringConst msg
              }
    modify $ \s -> s { moduleDefs = defs DS.|> def}


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


setLabel :: Name -> Named Terminator -> LLVM()
setLabel name t800 = do
    addBasicBlock t800
    modify $ \s -> s { blockName = name }


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


stringConst :: String -> C.Constant
stringConst msg = C.Array i8 [C.Int 8 (fromIntegral (ord c)) | c <- (msg ++ "\0")]    


local :: Type -> Name -> Operand
local = LocalReference


global :: Type -> Name -> C.Constant
global = C.GlobalReference


definedFunction :: Type -> Name -> Operand
definedFunction ty = ConstantOperand . (global ty)


alloca :: Maybe Operand -> Type -> String -> LLVM Operand
alloca cant ty r = do
    addNamedInstruction ty r $ Alloca ty cant 0 []


store :: Type -> Operand -> Operand -> LLVM Operand
store t ptr val =
    addUnNamedInstruction t $ Store False ptr val Nothing 0 []


load :: String -> Type -> LLVM (Operand)
load name ty = do 
    map <- gets varsLoc
    let i = fromJust $ DM.lookup name map
    addUnNamedInstruction ty $ Load False i Nothing 0 []


caller :: Type -> CallableOperand -> [(Operand, [ParameterAttribute])] -> LLVM Operand
caller ty df args = addUnNamedInstruction ty $ Call False CC.C [] df args [] []


branch :: Name -> Named Terminator
branch label = Do $ Br label [] 


condBranch :: Operand -> Name -> Name -> Named Terminator
condBranch op true false = Do $ CondBr op true false []


dimToOperand :: Either TE.Text Integer -> LLVM Operand
dimToOperand (Right n) = return $ ConstantOperand $ C.Int 32 n
dimToOperand (Left id) = load (TE.unpack id) intType


opsToArrayIndex :: String -> [Operand] -> LLVM (Operand)
opsToArrayIndex name ops = do
    arrD <- gets arrsDim
    let arrDims' = fromJust $ DM.lookup name arrD
    mulDims (tail arrDims') ops


-- Si la primera lista tiene mas elementos que la segunda paso al muy malo en el chequeo de tipos.
-- Significa que estas intentando acceder a una dimension del arreglo que no existe.
mulDims :: [Operand] -> [Operand] -> LLVM Operand
mulDims _ [acc] = return acc

mulDims (arrDim:xs) (acc:ys) = do
    op    <- mulDims xs ys
    opMul <- addUnNamedInstruction intType $ Mul False False arrDim acc []
    addUnNamedInstruction intType $ Add False False op opMul []


intType :: Type
intType = i32


intToDouble :: Operand -> LLVM Operand
intToDouble x = addUnNamedInstruction double $ SIToFP x double []


doubleToInt :: Operand -> LLVM Operand
doubleToInt x = addUnNamedInstruction i32 $ FPToSI x i32 [] 


retType :: Operand -> LLVM (Named Terminator)
retType op = do 
    n <- newLabel
    return $ n := Ret (Just op) []


retVoid :: LLVM (Named Terminator)
retVoid = do 
    n <- newLabel
    return $ n := Ret Nothing []


convertParams :: [(String, Contents SymbolTable)] -> [(String, Type)]
convertParams [] = []
convertParams ((id,c):xs) = 
    let t  = toType $ symbolType c in
      case procArgType $ c of
        T.In      -> (id, t) : convertParams xs
        otherwise -> (id, PointerType t (AddrSpace 0)) : convertParams xs


toType :: T.Type -> Type
toType T.MyInt   = i32
toType T.MyFloat = double
toType T.MyBool  = i1
toType T.MyChar  = i8
toType (T.MyArray _ t) = toType t 


voidType   = VoidType
boolType   = i1
doubleType = double
stringType = PointerType i8 (AddrSpace 0)
