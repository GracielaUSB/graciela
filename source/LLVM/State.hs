{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module LLVM.State where

--------------------------------------------------------------------------------

import           SymbolTable
import           AST.Struct
import qualified Type                         as T
--------------------------------------------------------------------------------
import           Control.Lens                 (makeLenses, use, (%=), (+=),
                                               (.=))
import           Control.Monad                (when)
import           Control.Monad.State          (MonadState, State)
-- import           Data.Char
import           Data.Foldable                (toList)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Monoid                  ((<>))
import           Data.Sequence                (Seq, (|>),ViewR((:>)), viewr)
import qualified Data.Sequence                as Seq
import           Data.Text                    (Text, unpack)
-- import           Data.Word
import           LLVM.General.AST             (BasicBlock (..))
import qualified LLVM.General.AST             as LLVM (Definition (..))
import           LLVM.General.AST.Instruction (Named (..), Terminator (..))
import qualified LLVM.General.AST.Instruction as LLVM (Instruction (..))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (CallableOperand, Operand (..))
--------------------------------------------------------------------------------

type Inst  = Named LLVM.Instruction
type Insts = Seq Inst

data LLVMState
  = LLVMState
    { _insCount      :: Word                        -- Cantidad de instrucciones sin nombre
    , _condName      :: Name
    , _blockName     :: Name                        -- Cantidad de bloques básicos en el programa
    , _currentBlock  :: Seq (Named LLVM.Instruction)-- Lista de instrucciones en el bloque básico actual
    , _blocks        :: Seq BasicBlock              -- Lista de bloques básicos en la definición actual
    , _moduleDefs    :: Seq LLVM.Definition
    , _symTable      :: [Map String String]
    , _nameCount     :: Int
    , _structs       :: Map Text Struct
    , _currentStruct :: Maybe Struct
    }

makeLenses ''LLVMState

newtype LLVM a = LLVM { unLLVM :: State LLVMState a }
  deriving (Functor, Applicative, Monad, MonadState LLVMState)


initialState :: LLVMState
initialState = LLVMState
  { _insCount      = 1
  , _condName      = UnName 0
  , _blockName     = UnName 0
  , _currentBlock  = Seq.empty
  , _blocks        = Seq.empty
  , _moduleDefs    = Seq.empty
  , _symTable      = []
  , _nameCount     = 0
  , _structs       = Map.empty
  , _currentStruct = Nothing 
  }

{- Symbol Table -}
-- When opening a new scope, llvm wont know which variable is beign called if more than 1 variable have the same name.
-- To prevent this confusion, lets call every declared variable with an unique name + identifier
-- (e.g. a -> %a1 and %a2)

getVariableName :: String -> LLVM String
getVariableName name = do
  st <- use symTable
  return $ getVariableName' st
  where
    getVariableName' [] = error $ "variable no definida " <> name
    getVariableName' (vars:xs) = case name `Map.lookup` vars of
      Just currentName -> currentName
      Nothing -> getVariableName' xs 

openScope :: LLVM ()
openScope = symTable %= (Map.empty :)

closeScope :: LLVM ()
closeScope = do
  t <- tail <$> use symTable
  when (null t) (nameCount .= 0)
  symTable .= t


insertName :: String -> LLVM String
insertName name = do 
  (vars:xs) <- use symTable
  id <- use nameCount
  let 
    newName = name <> "-" <> show id
    newMap = Map.insert name newName vars
  nameCount += 1
  symTable .= (newMap:xs)
  return newName

-----------------------------------------------

addDefinitions :: Seq LLVM.Definition -> LLVM ()
addDefinitions defs =
  moduleDefs %= (<> defs)

addDefinition :: LLVM.Definition -> LLVM ()
addDefinition defs =
  moduleDefs %= (|> defs)

addInstructions :: Insts -> LLVM ()
addInstructions insts =
  currentBlock %= (<> insts)

addInstruction :: Inst -> LLVM ()
addInstruction inst =
  currentBlock %= (|> inst)

addBlock :: Named Terminator -> LLVM ()
addBlock terminator = do
  insts <- use currentBlock
  currentBlock .= Seq.empty
  name  <- use blockName
  blocks %= (|> BasicBlock name (toList insts) terminator)
  name' <-  newLabel
  blockName .= name'

setLabel :: Name -> Named Terminator -> LLVM()
setLabel name terminator = do
    addBlock terminator
    blockName .= name

newLabel :: LLVM Name
newLabel = do
  count <- use insCount
  insCount += 1
  return $ UnName count


writeLnInt     = "_writeLnInt"
writeLnBool    = "_writeLnBool"
writeLnChar    = "_writeLnChar"
writeLnFloat   = "_writeLnDouble"
writeLnString  = "_writeLnString"
writeInt       = "_writeInt"
writeBool      = "_writeBool"
writeChar      = "_writeChar"
writeFloat     = "_writeDouble"
writeString    = "_writeString"
randomInt      = "_random"
sqrtString     = "llvm.sqrt.f64"
fabsString     = "llvm.fabs.f64"
powString      = "llvm.pow.f64"
minnumString   = "_min"
maxnumString   = "_max"
minnumFstring  = "_minF"
maxnumFstring  = "_maxF"
readIntStd     = "_readIntStd"
readCharStd    = "_readCharStd"
readFloatStd   = "_readDoubleStd"
openFileStr    = "_openFile"
readFileInt    = "_readFileInt"
closeFileStr   = "_closeFile"
readFileChar   = "_readFileChar"
readFileFloat  = "_readFileDouble"
intAdd         = "llvm.sadd.with.overflow.i32"
intSub         = "llvm.ssub.with.overflow.i32"
intMul         = "llvm.smul.with.overflow.i32"

concat' :: [Seq a] -> Seq a
concat' = foldr (<>) Seq.empty


-- emptyCodegen :: LLVMState
-- emptyCodegen = LLVMState 1 (UnName 0) (UnName 0) Seq.empty Seq.empty Seq.empty Map.empty Map.empty


-- execCodegen :: LLVM a -> LLVMState
-- execCodegen m = execState (unLLVM m) emptyCodegen



-- addDimToArray :: String -> Operand -> LLVM()
-- addDimToArray name op = do
--     dims <- use arrsDim
--     arrsDim .= Map.insertWith (++) name [op] dims





-- globalVariable :: Name -> Type -> C.Constant -> LLVM ()
-- globalVariable name t init = do
--     defs <- use moduleDefs
--     let def = GlobalDefinition $ globalVariableDefaults
--                 { name  = name
--                 , linkage = Private
--                 , Global.type' = t
--                 , initializer = Just init
--                 , isConstant  = False
--                 }
--     moduleDefs .= defs Seq.|> def


-- addBasicBlock :: Named Terminator -> LLVM ()
-- addBasicBlock t800 = do
--     lins  <- use instrs
--     bbl   <- use bblocs
--     name  <- use blockName
--     name' <- newLabel
--     blockName .= name'
--     instrs    .= Seq.empty
--     bblocs    .= bbl Seq.|> BasicBlock name (toList lins) t800


-- addNamedInstruction :: Type -> String -> Instruction -> LLVM Operand
-- addNamedInstruction t name ins = do
--     lins <- use instrs
--     let r = Name name
--     instrs .= lins Seq.|> (r := ins)
--     let op = local t r
--     addVarOperand name op
--     return op


-- addString :: String -> Name -> Type -> LLVM ()
-- addString msg name t = do
--     defs <- use moduleDefs
--     let def = GlobalDefinition $ globalVariableDefaults
--                 { name        = name
--                 , isConstant  = True
--                 , Global.type'  = t
--                 , initializer = Just $ constantString msg
--                 }
--     moduleDefs .= defs Seq.|> def


-- addFileName :: String -> Name -> Type -> LLVM ()
-- addFileName msg name t = do
--     defs <- use moduleDefs
--     let def = GlobalDefinition $ globalVariableDefaults
--                 { name         = name
--                 , isConstant   = True
--                 , Global.type' = t
--                 , initializer  = Just (constantFileName msg)
--                 }
--     moduleDefs .= defs Seq.|> def



-- addStringOpe :: String -> LLVM Operand
-- addStringOpe msg = do
--     let n  = fromIntegral $ Prelude.length msg+1
--     let t = ArrayType n i16
--     name <- newLabel
--     addString msg name t
--     return $ ConstantOperand $ C.GetElementPtr True (global i16 name) [C.Int 64 0, C.Int 64 0]


-- addFileNameOpe :: String -> LLVM Operand
-- addFileNameOpe msg = do
--     let n =  fromIntegral $ Prelude.length msg+1
--     let t =  ArrayType n i8
--     name  <- newLabel
--     addFileName msg name t
--     return $ ConstantOperand $ C.GetElementPtr True (global i8 name) [C.Int 64 0, C.Int 64 0]


-- setLabel :: Name -> Named Terminator -> LLVM()
-- setLabel name t800 = do
--     addBasicBlock t800
--     blockName .= name


-- checkVar :: String -> Type -> LLVM Operand
-- checkVar id t = do
--     vars <- use varsLoc
--     case Map.lookup id vars of
--       Just op -> return op
--       Nothing -> alloca Nothing t id


-- addVarOperand :: String -> Operand -> LLVM()
-- addVarOperand name op = do
--     map <- use varsLoc
--     varsLoc .= Map.insert name op map


-- getVarOperand :: String -> LLVM Operand
-- getVarOperand name = do
--     map <- use varsLoc
--     return $ fromJust $ Map.lookup name map


-- addUnNamedInstruction :: Type -> Instruction -> LLVM Operand
-- addUnNamedInstruction t ins = do
--     r    <- newLabel
--     instrs %= (Seq.|> (r := ins))
--     return $ local t r


-- getCount :: LLVM Word
-- getCount = do
--     n <- use insCount
--     insCount .= n + 1
--     return n


-- local :: Type -> Name -> Operand
-- local = LocalReference


-- global :: Type -> Name -> C.Constant
-- global = C.GlobalReference


-- constantInt :: Integer -> Operand
-- constantInt n = ConstantOperand $ C.Int 32 n


-- constantFloat :: Double -> Operand
-- constantFloat n = ConstantOperand $ C.Float $ Double n


-- constantBool :: Integer -> Operand
-- constantBool n = ConstantOperand $ C.Int 1 n


-- constantChar :: Char -> Operand
-- constantChar c = ConstantOperand $ C.Int 9 $ toInteger (ord c)


-- defaultChar :: Operand
-- defaultChar = ConstantOperand $ C.Int 9 1


-- constantString :: String -> C.Constant
-- constantString msg =
--    C.Array i16 [C.Int 16 (toInteger (ord c)) | c <- msg ++ "\0"]

-- constantFileName:: String -> C.Constant
-- constantFileName msg =
--    C.Array pointerType [C.Int 8 (toInteger (ord c)) | c <- msg ++ "\0"]


-- definedFunction :: Type -> Name -> Operand
-- definedFunction t = ConstantOperand . global t


-- initialize :: String -> T.Type -> LLVM Operand
-- initialize id T.GInt = do
--    op <- getVarOperand id
--    store intType op $ constantInt 0

-- initialize id T.GFloat = do
--    op <- getVarOperand id
--    store floatType op $ constantFloat 0.0

-- initialize id T.GBool = do
--    op <- getVarOperand id
--    store charType op $ constantBool 0

-- initialize id T.GChar = do
--    op <- getVarOperand id
--    store charType op defaultChar


-- alloca :: Maybe Operand -> Type -> String -> LLVM Operand
-- alloca cant t r = addNamedInstruction t r $ Alloca t cant 0 []


-- store :: Type -> Operand -> Operand -> LLVM Operand
-- store t ptr val =
--     addUnNamedInstruction t $ Store False ptr val Nothing 0 []


-- load :: String -> Type -> LLVM Operand
-- load name t = do
--     map <- use varsLoc
--     let i = fromJust $ Map.lookup name map
--     addUnNamedInstruction t $ Load False i Nothing 0 []


-- caller :: Type -> CallableOperand -> [(Operand, [ParameterAttribute])] -> LLVM Operand
-- caller t df args = addUnNamedInstruction t $ Call Nothing CC.C [] df args [] []


-- branch :: Name -> Named Terminator
-- branch label = Do $ Br label []


-- condBranch :: Operand -> Name -> Name -> Named Terminator
-- condBranch op true false = Do $ CondBr op true false []


-- nothing :: Named Terminator
-- nothing = Do $ Unreachable []


-- returnVal :: Operand -> Named Terminator
-- returnVal op = Do $ Ret (Just op) []


-- extracValue :: Operand -> Word32 -> LLVM Operand
-- extracValue name n = addUnNamedInstruction voidType $ ExtractValue name [n] []


-- dimToOperand :: Either Text Integer -> LLVM Operand
-- dimToOperand (Right n) = return $ ConstantOperand $ C.Int 32 n
-- dimToOperand (Left id) = load (unpack id) intType


-- opsToArrayIndex :: String -> [Operand] -> LLVM Operand
-- opsToArrayIndex name ops = do
--     arrD <- use arrsDim
--     let arrDims' = fromJust $ Map.lookup name arrD
--     mulDims (tail arrDims') ops


-- -- Si la primera lista tiene mas elementos que la segunda paso al muy malo en el chequeo de tipos.
-- -- Significa que estas intentando acceder a una dimension del arreglo que no existe.
-- mulDims :: [Operand] -> [Operand] -> LLVM Operand
-- mulDims _ [acc] = return acc

-- mulDims (arrDim:xs) (acc:ys) = do
--     op    <- mulDims xs ys
--     opMul <- addUnNamedInstruction intType $ Mul False False arrDim acc []
--     addUnNamedInstruction intType $ Add False False op opMul []


-- retVoid :: LLVM (Named Terminator)
-- retVoid = do
--     n <- newLabel
--     return $ n := Ret Nothing []


-- convertParams :: [(String, Contents SymbolTable)] -> [(String, Type)]
-- convertParams [] = []
-- convertParams ((id,c):xs) =
--     let t  = toType $ argType c
--     in case argTypeArg c of
--       T.In      -> (id, t) : convertParams xs
--       _         -> (id, PointerType t (AddrSpace 0)) : convertParams xs

-- convertFuncParams :: [(Text, T.Type)] -> [(String, Type)]
-- convertFuncParams [] = []
-- convertFuncParams ((id, T.GArray s t):xs) =
--     (unpack id, PointerType (toType t) (AddrSpace 0)) : convertFuncParams xs
-- convertFuncParams ((id, t):xs) =
--     (unpack id, toType t) : convertFuncParams xs
