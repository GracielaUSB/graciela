{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Language.Graciela.LLVM.State
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
  , freeArgInsts
  , doingFunction
  , currentStruct
  , stringIds
  , stringOps
  , boundOp
  , substitutionTable
  , doGet
  , coupling
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Struct (Struct (..))
import           Language.Graciela.AST.Type   (TypeArgs)
import           Language.Graciela.Common
--------------------------------------------------------------------------------
import           Control.Lens                 (makeLenses)
import           Data.Array                   (Array)
import qualified Data.Map.Strict              as Map (empty)
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
  , _freeArgInsts      :: Seq (Named Instruction)
  , _doingFunction     :: Bool
  , _blocks            :: Seq BasicBlock          -- Lista de bloques básicos en la definición actual
  , _moduleDefs        :: Seq Definition
  , _symTable          :: [Map Text Name]
  , _structs           :: Map Text Struct
  , _fullDataTypes     :: Map Text (Struct, Set TypeArgs)
  , _currentStruct     :: Maybe Struct
  , _stringIds         :: Map Text Int
  , _stringOps         :: Array Int Operand
  , _boundOp           :: Maybe Operand
  , _substitutionTable :: [TypeArgs]
  , _doGet             :: Bool
  , _coupling          :: Bool }


makeLenses ''State

initialState :: State
initialState = State
  { _nameSupply        = Map.empty
  , _unnameSupply      = 1
  , _blockName         = Nothing
  , _currentBlock      = Seq.empty
  , _freeArgInsts      = Seq.empty
  , _doingFunction     = False
  , _blocks            = Seq.empty
  , _moduleDefs        = Seq.empty
  , _symTable          = []
  , _structs           = Map.empty
  , _fullDataTypes     = Map.empty
  , _currentStruct     = Nothing
  , _stringIds         = Map.empty
  , _stringOps         = undefined
  , _boundOp           = Nothing
  , _substitutionTable = []
  , _doGet             = True
  , _coupling          = False }
