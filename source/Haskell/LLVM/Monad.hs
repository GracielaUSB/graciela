{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.Monad where
--------------------------------------------------------------------------------
import           Common
import           LLVM.State                       hiding (State)
import qualified LLVM.State                       as LLVM (State)
--------------------------------------------------------------------------------

import           Control.Lens                     (at, ix, use, (%=), (+=),
                                                   (.=), (<<+=), (?=), _head)
import           Control.Monad.State.Class        (MonadState)
import           Control.Monad.Trans.State.Strict (State)
import           Data.Foldable                    (toList)
import qualified Data.Map.Strict                  as Map (empty, insert, lookup)
import           Data.Maybe                       (fromMaybe)
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
    getVariableName' [] = Name "Error"--error $
      -- "internal error: undefined variable `" <> unpack name <> "`."

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

addArgInsts :: Inst -> LLVM ()
addArgInsts inst = 
  freeArgInsts %= (|> inst)
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
newLabel "" = internal "empty label, use newUnLabel"
newLabel label = do
  ns <- use nameSupply
  case label `Map.lookup` ns of
    Nothing -> do
      nameSupply . at label ?= 1
      pure . Name $ "." <> label
    Just i  -> do
      nameSupply . ix label %= succ
      pure . Name $ "." <> label <> "." <> show i

newUnLabel :: LLVM Name
newUnLabel = UnName <$> (unnameSupply <<+= 1)
--------------------------------------------------------------------------------

callable :: Type -> String -> Either a Operand
callable t = Right . ConstantOperand . GlobalReference t . Name

firstSetString, nextSetString :: String
firstSetString = "_firstSet"
nextSetString = "_nextSet"

copyArrayString :: String
copyArrayString = "_copyArray"

firstMultisetString, nextMultisetString :: String
firstMultisetString = "_firstMultiset"
nextMultisetString = "_nextMultiset"

firstSequenceString, nextSequenceString :: String
firstSequenceString = "_firstSequence"
nextSequenceString = "_nextSequence"

initTrashCollectorString, freeTrashCollectorString, openScopeString :: String
initTrashCollectorString = "_initTrashCollector"
freeTrashCollectorString = "_freeTrashCollector"
openScopeString = "_openScope"

newSetString, newSeqString, newMultisetString :: String
newSetString = "_newSet"
newSeqString = "_newSequence"
newMultisetString = "_newMultiset"

newSetPairString,newMultisetPairString, newSeqPairString :: String
newSetPairString      = "_newSetPair"
newMultisetPairString = "_newMultisetPair"
newSeqPairString      = "_newSequencePair"

equalSetString, equalSeqString, equalMultisetString :: String
equalSetString      = "_equalSet"
equalSeqString      = "_equalSequence"
equalMultisetString = "_equalMultiset"
equalFuncString     = "_equalFunction"
equalRelString      = "_equalRelation"
equalSetPairString      = "_equalSetPair"
equalSeqPairString      = "_equalSequencePair"
equalMultisetPairString = "_equalMultisetPair"
equalTupleString        = "_equalTuple"

evalFuncString, evalRelString :: String
evalFuncString     = "_pairFunction"
evalRelString      = "_pairRelation"

sizeSetString, sizeSeqString, sizeMultisetString, sizeRelString, sizeFuncString :: String
sizeSetString      = "_sizeSet"
sizeSeqString      = "_sizeSequence"
sizeMultisetString = "_sizeMultiset"
sizeRelString      = "_sizeRelation"
sizeFuncString     = "_sizeFunction"

supersetSetString, supersetMultisetString :: String
supersetSetString      = "_includesSet"
supersetMultisetString = "_includesMultiset"
supersetSetPairString      = "_includesSetPair"
supersetMultisetPairString = "_includesMultisetPair"

ssupersetSetString, ssupersetMultisetString :: String
ssupersetSetString      = "_includesSSet"
ssupersetMultisetString = "_includesSMultiset"
ssupersetSetPairString  = "_includesSSetPair"
ssupersetMultisetPairString = "_includesSMultisetPair"

insertSetString, insertSeqString, insertMultisetString :: String
insertSetString      = "_insertSet"
insertSeqString      = "_insertSequence"
insertMultisetString = "_insertMultiset"

insertSetPairString, insertMultisetPairString, insertSeqPairString:: String
insertSetPairString      = "_insertSetPair"
insertMultisetPairString = "_insertMultisetPair"
insertSeqPairString      = "_insertSequencePair"


isElemSetString, isElemMultisetString, isElemSeqString :: String
isElemSetString      = "_isElemSet"
isElemMultisetString = "_isElemMultiset"
isElemSeqString      = "_isElemSequence"
isElemSetPairString      = "_isElemSetPair"
isElemMultisetPairString = "_isElemMultisetPair"
isElemSeqPairString      = "_isElemSequencePair"


unionSetString, unionMultisetString :: String
unionSetString          = "_unionSet"
unionMultisetString     = "_unionMultiset"
unionSetPairString      = "_unionSetPair"
unionMultisetPairString = "_unionMultisetPair"
unionFunctionString     = "_unionFunction"

intersectSetString, intersectMultisetString :: String
intersectSetString          = "_intersectSet"
intersectMultisetString     = "_intersectMultiset"
intersectSetPairString      = "_intersectSetPair"
intersectMultisetPairString = "_intersectMultisetPair"
intersectFunctionString     = "_intersectFunction"

differenceSetString, differenceMultisetString :: String
differenceSetString          = "_differenceSet"
differenceMultisetString     = "_differenceMultiset"
differenceSetPairString      = "_differenceSetPair"
differenceMultisetPairString = "_differenceMultisetPair"
differenceFunctionString     = "_differenceFunction"

multisetSumString, concatSequenceString :: String
multisetSumString        = "_sumMultiset"
concatSequenceString     = "_concatSequence"
multisetPairSumString    = "_sumMultisetPair"
concatSequencePairString = "_concatSequencePair"
atSequenceString         = "_atSequence"
atSequencePairString     = "_atSequencePair"


freeString :: String
freeString = "_free"

mallocString :: String
mallocString   = "_malloc"
mallocTCString = "_mallocTC"

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
powIString  :: String
powIString  = "_powInt"

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
readBoolStd   :: String
readBoolStd   = "_readBoolStd"
readCharStd   :: String
readCharStd   = "_readCharStd"
readFloatStd  :: String
readFloatStd  = "_readDoubleStd"

openFileStr   :: String
openFileStr   = "_openFile"
readFileBool  :: String
readFileBool  = "_readFileBool"
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
