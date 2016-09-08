{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module LLVM.State
  ( State (..)
  , initialState
  , nameSupply
  , blockName
  , currentBlock
  , blocks
  , moduleDefs
  , symTable
  -- , nameCount
  , structs
  , currentStruct
  , stringOps
  , boundOp
  , substitutionTable
  ) where
--------------------------------------------------------------------------------
import           AST.Struct                   (Struct (..))
import qualified AST.Type                     as G (Type)
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
import           LLVM.General.AST.Type        (Type)
--------------------------------------------------------------------------------

type Inst  = Named Instruction

data State = State
  { _nameSupply        :: Map String Word
  , _blockName         :: Maybe Name              -- Cantidad de bloques básicos en el programa
  , _currentBlock      :: Seq (Named Instruction) -- Lista de instrucciones en el bloque básico actual
  , _blocks            :: Seq BasicBlock          -- Lista de bloques básicos en la definición actual
  , _moduleDefs        :: Seq Definition
  , _symTable          :: [Map Text Name]
  , _structs           :: Map Text Struct
  , _currentStruct     :: Maybe Struct
  , _stringOps         :: Array Int Operand
  , _boundOp           :: Maybe Operand
  , _substitutionTable :: [Map G.Type  G.Type] }

makeLenses ''State

initialState :: State
initialState = State
  { _nameSupply        = Map.empty
  , _blockName         = Nothing
  , _currentBlock      = Seq.empty
  , _blocks            = Seq.empty
  , _moduleDefs        = Seq.empty
  , _symTable          = []
  , _structs           = Map.empty
  , _currentStruct     = Nothing
  , _stringOps         = undefined
  , _boundOp           = Nothing
  , _substitutionTable = [] }
