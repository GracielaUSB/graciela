{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.Graciela.Parser.State
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
  , existsDT
  , dataTypes
  , fullDataTypes
  , pendingDataType
  , doingCoupleRel
  , initialState
  , stringIds
  , stringCounter
  , pragmas
  , isDeclarative
  , allowAbstNames
  , useLet
  , readFilesStack
  , readFiles

  , crName
  , crPos
  , crParams
  , crType
  , crRecAllowed
  , crRecursive
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Definition hiding (pragmas)
import           Language.Graciela.AST.Module     hiding (pragmas)
import           Language.Graciela.AST.Struct
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.Error
import           Language.Graciela.SymbolTable
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Control.Lens                     (makeLenses)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map (empty, fromList)
import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as Seq (empty)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set (empty)
import           Data.Text                        (Text, pack)
import           Text.Megaparsec.Error            (ParseError (..))
import           Text.Megaparsec.Pos              (unsafePos)
--------------------------------------------------------------------------------

type RecursionAllowed = Bool

data CurrentRoutine ps t = CurrentRoutine
  { _crName       :: Text
  , _crPos        :: SourcePos
  , _crParams     :: ps
  , _crType       :: t
  , _crTypeArgs   :: Maybe (Text,TypeArgs)
  , _crRecAllowed :: RecursionAllowed
  , _crRecursive  :: Bool }

makeLenses ''CurrentRoutine

type CurrentProc = CurrentRoutine (Seq (Text, Type, ArgMode)) ()
type CurrentFunc = CurrentRoutine (Seq (Text, Type)) Type

data State = State
  { _errors          :: Seq (ParseError TokenPos Error)
  , _symbolTable     :: SymbolTable
  , _definitions     :: Map Text Definition
  , _filesToRead     :: Set String
  , _currentProc     :: Maybe CurrentProc
  , _currentFunc     :: Maybe CurrentFunc
  , _currentStruct   :: Maybe (Type, Fields, Map Text Definition, Fields)
  , _typeVars        :: [Text]
  , _existsDT        :: Bool
  , _dataTypes       :: Map Text Struct
  , _fullDataTypes   :: Map Text (Map TypeArgs Bool)
  , _pendingDataType :: Map Text (Set Text)
  , _userDefinedType :: Set Type
  , _doingCoupleRel  :: Bool
  , _stringIds       :: Map Text Int
  , _stringCounter   :: Int
  , _pragmas         :: Set Pragma
  , _isDeclarative   :: Bool
  , _allowAbstNames  :: Bool
  , _useLet          :: Bool
  , _readFilesStack  :: [String]
  , _readFiles       :: Map String Module }

makeLenses ''State

initialState :: Set Pragma -> State
initialState pragmas = State
  { _errors          = Seq.empty
  , _symbolTable     = emptyGlobal
  , _definitions     = Map.empty
  , _filesToRead     = Set.empty
  , _currentProc     = Nothing
  , _currentFunc     = Nothing
  , _currentStruct   = Nothing
  , _typeVars        = []
  , _existsDT        = True
  , _dataTypes       = Map.empty
  , _fullDataTypes   = Map.empty
  , _doingCoupleRel  = False
  , _pendingDataType = Map.empty
  , _userDefinedType = Set.empty
  , _stringIds       = Map.empty
  , _stringCounter   = 0
  , _pragmas         = pragmas
  , _isDeclarative   = LogicAnywhere `elem` pragmas
  , _allowAbstNames  = False
  , _useLet          = False
  , _readFilesStack = []
  , _readFiles     = Map.empty }
