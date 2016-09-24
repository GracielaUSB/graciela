{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Parser.State
  ( State (..)
  , CurrentRoutine (..)
  , CurrentFunc
  , CurrentProc
  , errors
  , symbolTable
  , filesToRead
  , definitions
  , currentProc
  , currentFunc
  , currentStruct
  , typeVars
  , dataTypes
  , fullDataTypes
  , initialState
  , stringIds
  , crName
  , crPos
  , crParams
  , crType
  , crRecAllowed
  , crRecursive
  ) where
--------------------------------------------------------------------------------
import           AST.Definition
import           AST.Expression        (Value (BoolV))
import           AST.Struct
import           AST.Type
import           Entry
import           Error
import           Location
import           SymbolTable
import           Token
--------------------------------------------------------------------------------
import           Control.Lens          (makeLenses)
import           Data.Foldable         (foldl')
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map (empty, fromList)
import           Data.Sequence         (Seq)
import qualified Data.Sequence         as Seq (empty)
import           Data.Set              (Set)
import qualified Data.Set              as Set (empty)
import           Data.Text             (Text, pack)
import           Text.Megaparsec.Error (ParseError (..))
import           Text.Megaparsec.Pos   (unsafePos)
--------------------------------------------------------------------------------

type RecursionAllowed = Bool

data CurrentRoutine ps t = CurrentRoutine
  { _crName       :: Text
  , _crPos        :: SourcePos
  , _crParams     :: ps
  , _crType       :: t
  , _crRecAllowed :: RecursionAllowed
  , _crRecursive  :: Bool }

makeLenses ''CurrentRoutine

type CurrentProc = CurrentRoutine (Seq (Text, Type, ArgMode)) ()
type CurrentFunc = CurrentRoutine (Seq (Text, Type)) Type

data State = State
  { _errors        :: Seq (ParseError TokenPos Error)
  , _symbolTable   :: SymbolTable
  , _definitions   :: Map Text Definition
  , _filesToRead   :: Set String
  , _currentProc   :: Maybe CurrentProc
  , _currentFunc   :: Maybe CurrentFunc
  , _currentStruct :: Maybe (Type, Fields, Map Text Definition)
  , _typeVars      :: [Text]
  , _dataTypes     :: Map Text Struct
  , _fullDataTypes :: Map Text (Struct, [TypeArgs]) {-Struct)-}
  , _stringIds     :: Map Text Int }

makeLenses ''State


initialState :: FilePath -> State
initialState path  = State
  { _errors        = Seq.empty
  , _symbolTable   = emptyGlobal
  , _definitions   = Map.empty
  , _filesToRead   = Set.empty
  , _currentProc   = Nothing
  , _currentFunc   = Nothing
  , _typeVars      = []
  , _dataTypes     = Map.empty
  , _fullDataTypes = Map.empty
  , _currentStruct = Nothing
  , _stringIds     = Map.empty
  }
