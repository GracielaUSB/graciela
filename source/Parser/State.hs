{-# LANGUAGE TemplateHaskell #-}

module Parser.State
  ( State
  , synErrorList
  , errors
  , symbolTable
  , filesToRead
  , currentProc
  , typesTable
  , recSet
  , initialState
  ) where
--------------------------------------------------------------------------------
import           Error
import           Location
import           SymbolTable
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Lens          (makeLenses)
import           Data.Map              (Map)
import qualified Data.Map              as Map (fromList)
import           Data.Sequence         (Seq)
import qualified Data.Sequence         as Seq (empty)
import           Data.Set              (Set)
import qualified Data.Set              as Set (empty)
import           Data.Text             (Text, pack)
import           Text.Megaparsec.Error (ParseError (..))
--------------------------------------------------------------------------------

data State = State
  { _synErrorList :: Seq MyParseError
  , _errors       :: Seq (ParseError TokenPos Error)
  , _symbolTable  :: SymbolTable
  , _filesToRead  :: Set String
  , _currentProc  :: Maybe (Text, SourcePos, [(Text,Type)])
  , _typesTable   :: Map Text (Type, SourcePos)
  , _recSet       :: [Token]
  }

makeLenses ''State


initialState :: State
initialState = State
  { _synErrorList    = Seq.empty
  , _errors          = Seq.empty
  , _symbolTable     = empty gracielaDef
  , _filesToRead     = Set.empty
  , _currentProc     = Nothing
  , _typesTable      = initialTypes
  , _recSet          = []
  }
  where
    initialTypes = Map.fromList
      [ (pack "int",    (GInt,   gracielaDef))
      , (pack "float",  (GFloat, gracielaDef))
      , (pack "boolean",(GBool,  gracielaDef))
      , (pack "char",   (GChar,  gracielaDef))
      ]
    gracielaDef = SourcePos "graciela.def" (unsafePos 1) (unsafePos 1)
