{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module LLVM.CodegenState where

--------------------------------------------------------------------------------
import           Contents
import           SymbolTable
import qualified Type                               as T
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens                       (makeLenses, use, (%=), (.=))
import           Control.Monad.State
import           Data.Char
import           Data.Foldable                      (toList)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe
import           Data.Sequence                      (Seq)
import qualified Data.Sequence                      as Seq
import           Data.Text                          (Text, unpack)
import           Data.Word
import           LLVM.General.AST                   as AST
import           LLVM.General.AST.AddrSpace
import           LLVM.General.AST.Attribute
import           LLVM.General.AST.Float
import           LLVM.General.AST.Global            as Global
import           LLVM.General.AST.Linkage
import           LLVM.General.AST.Type
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
--------------------------------------------------------------------------------


data CodegenSt
  = CodeGenSt
    { _insCount   :: Word                        -- Cantidad de instrucciones sin nombre
    , _condName   :: Name
    , _blockName  :: Name                        -- Cantidad de bloques básicos en el programa
    , _instrs     :: Seq (Named Instruction)     -- Lista de instrucciones en el bloque básico actual
    , _bblocs     :: Seq BasicBlock              -- Lista de bloques básicos en la definición actual
    , _moduleDefs :: Seq Definition
    , _varsLoc    :: Map String Operand
    , _arrsDim    :: Map String [Operand]
    } deriving (Show)

makeLenses ''CodegenSt

newtype LLVM a = LLVM { unLLVM :: State CodegenSt a }
  deriving (Functor, Applicative, Monad, MonadState CodegenSt)


emptyCodegen :: CodegenSt
emptyCodegen = CodeGenSt 1 (UnName 0) (UnName 0) Seq.empty Seq.empty Seq.empty Map.empty Map.empty


execCodegen :: LLVM a -> CodegenSt
execCodegen m = execState (unLLVM m) emptyCodegen


newLabel :: LLVM (Name)
newLabel = do
    n <- getCount
    return $ UnName n


addDimToArray :: String -> Operand -> LLVM()
addDimToArray name op = do
    dims <- use arrsDim
    arrsDim .= Map.insertWith (++) name [op] dims


addDefinition :: String -> ([Parameter], Bool) -> Type -> LLVM ()
addDefinition name params retTy = do
    bbl  <- use bblocs
    defs <- use moduleDefs
    let def = GlobalDefinition $ functionDefaults
                { name        = Name name
                , parameters  = params
                , returnType  = retTy
                , basicBlocks = (toList bbl)
                }
    bblocs     .= Seq.empty 
    varsLoc    .= Map.empty 
    moduleDefs .= defs Seq.|> def


globalVariable :: Name -> Type -> C.Constant -> LLVM ()
globalVariable name t init = do
    defs <- use moduleDefs
    let def = GlobalDefinition $ globalVariableDefaults
                { name  = name
                , linkage = Private
                , Global.type' = t
                , initializer = Just init
                , isConstant  = False
                }
    moduleDefs .= defs Seq.|> def


addBasicBlock :: Named Terminator -> LLVM ()
addBasicBlock t800 = do
    lins  <- use instrs
    bbl   <- use bblocs
    name  <- use blockName
    name' <- newLabel
    blockName .= name'
    instrs    .= Seq.empty
    bblocs    .= bbl Seq.|> BasicBlock name (toList lins) t800


addNamedInstruction :: Type -> String -> Instruction -> LLVM (Operand)
addNamedInstruction t name ins = do
    lins <- use instrs
    let r = Name name
    instrs .= lins Seq.|> (r := ins)
    let op = local t r
    addVarOperand name op
    return op


addString :: String -> Name -> Type -> LLVM ()
addString msg name t = do
    defs <- use moduleDefs
    let def = GlobalDefinition $ globalVariableDefaults
                { name        = name
                , isConstant  = True
                , Global.type'  = t
                , initializer = Just $ constantString msg
                }
    moduleDefs .= defs Seq.|> def


addFileName :: String -> Name -> Type -> LLVM ()
addFileName msg name t = do
    defs <- use moduleDefs
    let def = GlobalDefinition $ globalVariableDefaults
                { name         = name
                , isConstant   = True
                , Global.type' = t
                , initializer  = Just (constantFileName msg)
                }
    moduleDefs .= defs Seq.|> def



addStringOpe :: String -> LLVM (Operand)
addStringOpe msg = do
    let n  = fromIntegral $ Prelude.length msg+1
    let t = ArrayType n i16
    name <- newLabel
    addString msg name t
    return $ ConstantOperand $ C.GetElementPtr True (global i16 name) [C.Int 64 0, C.Int 64 0]


addFileNameOpe :: String -> LLVM (Operand)
addFileNameOpe msg = do
    let n =  fromIntegral $ Prelude.length msg+1
    let t =  ArrayType n i8
    name  <- newLabel
    addFileName msg name t
    return $ ConstantOperand $ C.GetElementPtr True (global i8 name) [C.Int 64 0, C.Int 64 0]


setLabel :: Name -> Named Terminator -> LLVM()
setLabel name t800 = do
    addBasicBlock t800
    blockName .= name


checkVar :: String -> Type -> LLVM Operand
checkVar id t = do
    vars <- use varsLoc
    case Map.lookup id vars of
      Just op -> return op
      Nothing -> do op <- alloca Nothing t id
                    return op


addVarOperand :: String -> Operand -> LLVM()
addVarOperand name op = do
    map <- use varsLoc
    varsLoc .= Map.insert name op map


getVarOperand :: String -> LLVM Operand
getVarOperand name = do
    map <- use varsLoc
    return $ fromJust $ Map.lookup name map


addUnNamedInstruction :: Type -> Instruction -> LLVM (Operand)
addUnNamedInstruction t ins = do
    r    <- newLabel
    lins <- use instrs
    instrs .= lins Seq.|> (r := ins)
    return $ local t r


getCount :: LLVM Word
getCount = do
    n <- use insCount
    insCount .= n + 1
    return $ n


local :: Type -> Name -> Operand
local = LocalReference


global :: Type -> Name -> C.Constant
global = C.GlobalReference


constantInt :: Integer -> Operand
constantInt n = ConstantOperand $ C.Int 32 n


constantFloat :: Double -> Operand
constantFloat n = ConstantOperand $ C.Float $ Double n


constantBool :: Integer -> Operand
constantBool n = ConstantOperand $ C.Int 1 n


constantChar :: Char -> Operand
constantChar c = ConstantOperand $ C.Int 9 $ toInteger (ord c)


defaultChar :: Operand
defaultChar = ConstantOperand $ C.Int 9 1


constantString :: String -> C.Constant
constantString msg =
   C.Array i16 [C.Int 16 (toInteger (ord c)) | c <- (msg ++ "\0")]

constantFileName:: String -> C.Constant
constantFileName msg =
   C.Array pointerType [C.Int 8 (toInteger (ord c)) | c <- (msg ++ "\0")]


definedFunction :: Type -> Name -> Operand
definedFunction t = ConstantOperand . (global t)


initialize :: String -> T.Type -> LLVM (Operand)
initialize id (T.GInt) = do
   op <- getVarOperand id
   store intType op $ constantInt 0

initialize id (T.GFloat) = do
   op <- getVarOperand id
   store floatType op $ constantFloat 0.0

initialize id (T.GBoolean) = do
   op <- getVarOperand id
   store charType op $ constantBool 0

initialize id (T.GChar) = do
   op <- getVarOperand id
   store charType op $ defaultChar


alloca :: Maybe Operand -> Type -> String -> LLVM Operand
alloca cant t r = do
    addNamedInstruction t r $ Alloca t cant 0 []


store :: Type -> Operand -> Operand -> LLVM Operand
store t ptr val =
    addUnNamedInstruction t $ Store False ptr val Nothing 0 []


load :: String -> Type -> LLVM (Operand)
load name t = do
    map <- use varsLoc
    let i = fromJust $ Map.lookup name map
    addUnNamedInstruction t $ Load False i Nothing 0 []


caller :: Type -> CallableOperand -> [(Operand, [ParameterAttribute])] -> LLVM Operand
caller t df args = addUnNamedInstruction t $ Call Nothing CC.C [] df args [] []


branch :: Name -> Named Terminator
branch label = Do $ Br label []


condBranch :: Operand -> Name -> Name -> Named Terminator
condBranch op true false = Do $ CondBr op true false []


nothing :: Named Terminator
nothing = (Do $ Unreachable [])


returnVal :: Operand -> Named Terminator
returnVal op = Do $ Ret (Just op) []


extracValue :: Operand -> Word32 -> LLVM Operand
extracValue name n = addUnNamedInstruction voidType $ ExtractValue name [n] []


dimToOperand :: Either Text Integer -> LLVM Operand
dimToOperand (Right n) = return $ ConstantOperand $ C.Int 32 n
dimToOperand (Left id) = load (unpack id) intType


opsToArrayIndex :: String -> [Operand] -> LLVM (Operand)
opsToArrayIndex name ops = do
    arrD <- use arrsDim
    let arrDims' = fromJust $ Map.lookup name arrD
    mulDims (tail arrDims') ops


-- Si la primera lista tiene mas elementos que la segunda paso al muy malo en el chequeo de tipos.
-- Significa que estas intentando acceder a una dimension del arreglo que no existe.
mulDims :: [Operand] -> [Operand] -> LLVM Operand
mulDims _ [acc] = return acc

mulDims (arrDim:xs) (acc:ys) = do
    op    <- mulDims xs ys
    opMul <- addUnNamedInstruction intType $ Mul False False arrDim acc []
    addUnNamedInstruction intType $ Add False False op opMul []


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
    let t  = toType $ argType c in

    case argTypeArg c of
      T.In      -> (id, t) : convertParams xs
      otherwise -> (id, PointerType t (AddrSpace 0)) : convertParams xs

convertFuncParams :: [(Text, T.Type)] -> [(String, Type)]
convertFuncParams [] = []
convertFuncParams ((id, (T.GArray s t)):xs) =
    (unpack id, PointerType (toType t) (AddrSpace 0)) : convertFuncParams xs
convertFuncParams ((id, t):xs) =
    (unpack id, toType t) : convertFuncParams xs

floatType :: Type
floatType = double

intType :: Type
intType = i32

charType :: Type
charType = IntegerType 9

pointerType :: Type
pointerType = IntegerType 8

voidType :: Type
voidType   = VoidType

boolType :: Type
boolType   = i1

doubleType :: Type
doubleType = double

stringType :: Type
stringType = PointerType i16 (AddrSpace 0)


toType :: T.Type -> Type
toType (T.GInt)         = intType
toType (T.GFloat)       = floatType
toType (T.GBoolean)        = boolType
toType (T.GChar)        = charType
toType (T.GArray _ t) = toType t

