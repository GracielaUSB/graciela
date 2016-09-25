{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.Monad where
--------------------------------------------------------------------------------
import           LLVM.State                       hiding (State)
import qualified LLVM.State                       as LLVM (State)
--------------------------------------------------------------------------------
import           Control.Lens                     (use, (%=), (+=), (.=), _head)
import           Control.Monad                    (when)
import           Control.Monad.State.Class        (MonadState)
import           Control.Monad.Trans.State.Strict (State)
import           Data.Foldable                    (toList)
import qualified Data.Map.Strict                  as Map (empty, insert, lookup)
import           Data.Maybe                       (fromMaybe)
import           Data.Semigroup                   ((<>))
import           Data.Sequence                    (Seq, (|>))
import qualified Data.Sequence                    as Seq
import           Data.Text                        (Text, unpack)
import           Data.Word                        (Word32)
import           LLVM.General.AST                 (BasicBlock (..))
import qualified LLVM.General.AST                 as LLVM (Definition (..))
import           LLVM.General.AST.Constant        (Constant (GlobalReference))
import           LLVM.General.AST.Instruction     (Named (..), Terminator (..))
import qualified LLVM.General.AST.Instruction     as LLVM (Instruction (..))
import           LLVM.General.AST.Name            (Name (..))
import           LLVM.General.AST.Operand         (Operand (ConstantOperand))
import           LLVM.General.AST.Type            (Type)
--------------------------------------------------------------------------------

type Inst  = Named LLVM.Instruction
type Insts = Seq Inst

newtype LLVM a = LLVM { unLLVM :: State LLVM.State a }
  deriving ( Functor, Applicative, Monad, MonadState LLVM.State)

{- Symbol Table -}
-- When opening a new scope, llvm wont know which variable is being called
-- if more than 1 variable have the same name. To prevent this confusion,
-- lets call every declared variable with an unique name + identifier
-- (e.g. a -> %a1 and %a2)

getVariableName :: Text -> LLVM Name
getVariableName name =
  getVariableName' <$> use symTable
  where
    getVariableName' [] = error $
      "internal error: undefined variable `" <> unpack name <> "`."
    getVariableName' (vars:xs) =
      fromMaybe (getVariableName' xs) (name `Map.lookup` vars)


openScope :: LLVM ()
openScope = symTable %= (Map.empty :)


closeScope :: LLVM ()
closeScope = symTable %= tail


insertVar :: Text -> LLVM Name
insertVar text = do
  name <- newLabel ("var." <> unpack text)
  symTable . _head %= Map.insert text name
  pure name
--------------------------------------------------------------------------------

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
--------------------------------------------------------------------------------

terminate :: Terminator -> LLVM ()
terminate terminator = do
  name' <- use blockName
  case name' of
    Nothing -> error $
      "internal error: attempted to terminate an unnamed block with\n" <>
      show (Do terminator) <> "\n"
    Just name -> do
      insts <- use currentBlock
      blocks %= (|> BasicBlock name (toList insts) (Do terminator))
      currentBlock .= Seq.empty
      blockName .= Nothing

(#) :: Name -> LLVM ()
(#) name = do
  old <- use blockName
  case old of
    Nothing -> blockName .= Just name
    Just oldName  -> error $
      "internal error: attempted to rename current bloc, " <> show oldName <>
      " as " <> show name <> "."
--------------------------------------------------------------------------------

newLabel :: String -> LLVM Name
newLabel label = do
  ns <- use nameSupply
  case label `Map.lookup` ns of
    Nothing -> do
      nameSupply %= Map.insert label 1
      pure . Name $ "." <> label
    Just i  -> do
      nameSupply %= Map.insert label (succ i)
      pure . Name $ "." <> label <> "." <> show i

newUnLabel :: LLVM Name
newUnLabel = newLabel ""
--------------------------------------------------------------------------------

callable :: Type -> String -> Either a Operand
callable t = Right . ConstantOperand . GlobalReference t . Name

initTrashCollectorString :: String
initTrashCollectorString = "_initTrashCollector"
freeTrashCollectorString :: String
freeTrashCollectorString = "_freeTrashCollector"
openScopeString :: String
openScopeString = "_openScope"

newSetString :: String
newSetString = "_newSet"
newSeqString :: String
newSeqString = "_newSequence"
newMultisetString :: String
newMultisetString = "_newMultiset"

equalSetString :: String
equalSetString = "_equalSet"
equalSeqString :: String
equalSeqString = "_equalSequence"
equalMultisetString :: String
equalMultisetString = "_equalMultiset"

subsetSetString :: String
subsetSetString = "_subsetSet"
subsetMultisetString :: String
subsetMultisetString = "_subsetMultiset"

ssubsetSetString :: String
ssubsetSetString = "_ssubsetSet"
ssubsetMultisetString :: String
ssubsetMultisetString = "_ssubsetMultiset"

insertSetString :: String
insertSetString = "_insertSet"
insertSeqString :: String
insertSeqString = "_insertSequence"
insertMultisetString :: String
insertMultisetString = "_insertMultiset"

isElemSetString :: String
isElemSetString = "_isElemSet"
isElemMultisetString :: String
isElemMultisetString = "_isElemMultiset"
isElemSeqString :: String
isElemSeqString = "_isElemSeq"

freeString    :: String
freeString    = "_free"
mallocString  :: String
mallocString  = "_malloc"

lnString      :: String
lnString      = "_ln"
writeIString  :: String
writeIString  = "_writeInt"
writeBString  :: String
writeBString  = "_writeBool"
writeCString  :: String
writeCString  = "_writeChar"
writeFString  :: String
writeFString  = "_writeDouble"
writeSString  :: String
writeSString  = "_writeString"

randomInt     :: String
randomInt     = "_random"

sqrtString    :: String
sqrtString    = "llvm.sqrt.f64"
fabsString    :: String
fabsString    = "llvm.fabs.f64"
powString     :: String
powString     = "llvm.pow.f64"

minnumString  :: String
minnumString  = "_min"
maxnumString  :: String
maxnumString  = "_max"
minnumFstring :: String
minnumFstring = "_minF"
maxnumFstring :: String
maxnumFstring = "_maxF"

readIntStd    :: String
readIntStd    = "_readIntStd"
readCharStd   :: String
readCharStd   = "_readCharStd"
readFloatStd  :: String
readFloatStd  = "_readDoubleStd"

openFileStr   :: String
openFileStr   = "_openFile"
readFileInt   :: String
readFileInt   = "_readFileInt"
closeFileStr  :: String
closeFileStr  = "_closeFile"
readFileChar  :: String
readFileChar  = "_readFileChar"
readFileFloat :: String
readFileFloat = "_readFileDouble"

safeAdd       :: Word32 -> String
safeAdd n     = "llvm.sadd.with.overflow.i" <> show n
safeSub       :: Word32 -> String
safeSub n     = "llvm.ssub.with.overflow.i" <> show n
safeMul       :: Word32 -> String
safeMul n     = "llvm.smul.with.overflow.i" <> show n
