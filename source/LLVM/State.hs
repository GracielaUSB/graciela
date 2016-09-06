{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module LLVM.State
  ( State (..)
  , initialState
  , nameSupply
  -- , condName
  , blockName
  , currentBlock
  , blocks
  , moduleDefs
  , varsLoc
  , arrsDim
  , outerBlock
  ) where
--------------------------------------------------------------------------------
import           Control.Lens                 (makeLenses)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map (empty)
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           LLVM.General.AST             (BasicBlock (..))
import qualified LLVM.General.AST             as LLVM (Definition (..))
import           LLVM.General.AST.Instruction (Named (..))
import qualified LLVM.General.AST.Instruction as LLVM (Instruction (..))
import           LLVM.General.AST.Name        (Name (..))
import           LLVM.General.AST.Operand     (Operand (..))
--------------------------------------------------------------------------------

data State
  = State
    {
      -- _insCount     :: Word                         -- Cantidad de instrucciones sin nombre
      _nameSupply   :: Map String Word
    -- , _condName     :: Name
    , _blockName    :: Maybe Name                   -- Cantidad de bloques básicos en el programa
    , _currentBlock :: Seq (Named LLVM.Instruction) -- Lista de instrucciones en el bloque básico actual
    , _blocks       :: Seq BasicBlock               -- Lista de bloques básicos en la definición actual
    , _moduleDefs   :: Seq LLVM.Definition
    , _varsLoc      :: Map String Operand
    , _arrsDim      :: Map String [Operand]
    , _outerBlock   :: Bool
    } deriving (Show)

makeLenses ''State

initialState :: State
initialState = State
  { _nameSupply   = Map.empty
  -- , _condName     = UnName 0
  , _blockName    = Nothing
  , _currentBlock = Seq.empty
  , _blocks       = Seq.empty
  , _moduleDefs   = Seq.empty
  , _varsLoc      = Map.empty
  , _arrsDim      = Map.empty
  , _outerBlock  = True
  }
