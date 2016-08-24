{-# LANGUAGE TemplateHaskell #-}

module Parser.State
  ( State
  , synErrorList
  , errors
  , symbolTable
  , filesToRead
  , currentProc
  , typesTable
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
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map (fromList)
import           Data.Sequence         (Seq)
import qualified Data.Sequence         as Seq (empty)
import           Data.Set              (Set)
import qualified Data.Set              as Set (empty)
import           Data.Text             (Text, pack)
import           Text.Megaparsec.Error (ParseError (..))
import           Text.Megaparsec.Pos   (unsafePos)
--------------------------------------------------------------------------------

data State = State
  { _synErrorList :: Seq MyParseError
  , _errors       :: Seq (ParseError TokenPos Error)
  , _symbolTable  :: SymbolTable
  , _filesToRead  :: Set String
  , _currentProc  :: Maybe (Text, SourcePos, [(Text,Type)])
  , _typesTable   :: Map Text (Type, Location)
  }

makeLenses ''State


initialState :: FilePath -> State
initialState path = State
  { _synErrorList    = Seq.empty
  , _errors          = Seq.empty
  , _symbolTable     = empty (SourcePos path (unsafePos 0) (unsafePos 0))
  , _filesToRead     = Set.empty
  , _currentProc     = Nothing
  , _typesTable      = initialTypes
  }
  where
    initialTypes = Map.fromList
      [ (pack "int",    (GInt,   GracielaDef))
      , (pack "float",  (GFloat, GracielaDef))
      , (pack "boolean",(GBool,  GracielaDef))
      , (pack "char",   (GChar,  GracielaDef))
      ]
