{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module LLVM.State
  ( State (..)
  , initialState
  , nameSupply
  , unnameSupply
  , blockName
  , currentBlock
  , blocks
  , moduleDefs
  , symTable
  , structs
  , fullDataTypes
  , pendingDataTypes
  , currentStruct
  , stringOps
  , boundOp
  , substitutionTable
  ) where
--------------------------------------------------------------------------------
import           AST.Struct                   (Struct (..))
import           AST.Type                     (TypeArgs)
--------------------------------------------------------------------------------
import           Control.Lens                 (makeLenses)
import           Data.Array                   (Array)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map (empty)
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Text                    (Text)
import           LLVM.General.AST             (BasicBlock (..), Definition (..))
import           LLVM.General.AST.Instruction (Instruction (..), Named (..))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (Operand)
--------------------------------------------------------------------------------

type Inst  = Named Instruction

data State = State
  { _nameSupply        :: Map String Word
  , _unnameSupply      :: Word
  , _blockName         :: Maybe Name              -- Cantidad de bloques básicos en el programa
  , _currentBlock      :: Seq (Named Instruction) -- Lista de instrucciones en el bloque básico actual
  , _blocks            :: Seq BasicBlock          -- Lista de bloques básicos en la definición actual
  , _moduleDefs        :: Seq Definition
  , _symTable          :: [Map Text Name]
  , _structs           :: Map Text Struct
  , _fullDataTypes     :: Map Text (Struct, [TypeArgs])
  , _pendingDataTypes  :: Map Text (Struct, [TypeArgs])
  , _currentStruct     :: Maybe Struct
  , _stringOps         :: Array Int Operand
  , _boundOp           :: Maybe Operand
  , _substitutionTable :: [TypeArgs] }

makeLenses ''State

initialState :: State
initialState = State
  { _nameSupply        = Map.empty
  , _unnameSupply      = 1
  , _blockName         = Nothing
  , _currentBlock      = Seq.empty
  , _blocks            = Seq.empty
  , _moduleDefs        = Seq.empty
  , _symTable          = []
  , _structs           = Map.empty
  , _fullDataTypes     = Map.empty
  , _pendingDataTypes  = Map.empty
  , _currentStruct     = Nothing
  , _stringOps         = undefined
  , _boundOp           = Nothing
  , _substitutionTable = [] }
