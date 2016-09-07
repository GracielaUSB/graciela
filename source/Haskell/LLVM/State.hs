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
  , nameCount
  , structs
  , currentStruct
  ) where

--------------------------------------------------------------------------------
import           AST.Struct                   (Struct (..))
import           SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens                 (makeLenses)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map (empty)
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           Data.Text                    (Text)
import           LLVM.General.AST             (BasicBlock (..))
import qualified LLVM.General.AST             as LLVM (Definition (..))
import           LLVM.General.AST.Instruction (Named (..))
import qualified LLVM.General.AST.Instruction as LLVM (Instruction (..))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (Operand (..))
--------------------------------------------------------------------------------

type Inst  = Named LLVM.Instruction
type Insts = Seq Inst

data State
  = State
    { _nameSupply    :: Map String Word
    , _blockName     :: Maybe Name                   -- Cantidad de bloques básicos en el programa
    , _currentBlock  :: Seq (Named LLVM.Instruction) -- Lista de instrucciones en el bloque básico actual
    , _blocks        :: Seq BasicBlock               -- Lista de bloques básicos en la definición actual
    , _moduleDefs    :: Seq LLVM.Definition
    , _symTable      :: [Map String String]
    , _nameCount     :: Int
    , _structs       :: Map Text Struct
    , _currentStruct :: Maybe Struct }

makeLenses ''State

initialState :: State
initialState = State
  { _nameSupply   = Map.empty
  , _blockName    = Nothing
  , _currentBlock = Seq.empty
  , _blocks       = Seq.empty
  , _moduleDefs   = Seq.empty
  , _symTable      = []
  , _nameCount     = -1
  , _structs       = Map.empty
  , _currentStruct = Nothing
  }
