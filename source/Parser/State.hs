{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Parser.State
  ( State (..)
  , errors
  , symbolTable
  , filesToRead
  , definitions
  , currentProc
  , currentFunc
  , typesTable
  , initialState
  ) where
--------------------------------------------------------------------------------
import           AST.Definition
import           AST.Expression        (Value (BoolV))
import           Entry
import           Error
import           Location
import           SymbolTable
import           Token
import           Type
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

data State = State
  { _errors       :: Seq (ParseError TokenPos Error)
  , _symbolTable  :: SymbolTable
  , _definitions  :: Map Text Definition
  , _filesToRead  :: Set String
  , _currentProc  ::
      Maybe (Text, SourcePos, Seq (Text, Type, ArgMode), RecursionAllowed)
  , _currentFunc  ::
      Maybe (Text, SourcePos, Type, Seq (Text, Type), RecursionAllowed)
  , _typesTable   :: Map Text (Type, Location)
  }

makeLenses ''State


initialState :: FilePath -> State
initialState path = State
  { _errors       = Seq.empty
  , _symbolTable
  , _definitions  = Map.empty
  , _filesToRead  = Set.empty
  , _currentProc  = Nothing
  , _currentFunc  = Nothing
  , _typesTable   = initialTypes
  }
  where
    initialTypes = Map.fromList
      [ (pack "int",    (GInt,   GracielaDef))
      , (pack "float",  (GFloat, GracielaDef))
      , (pack "boolean",(GBool,  GracielaDef))
      , (pack "char",   (GChar,  GracielaDef))
      ]
    symbols =
      [ ("otherwise", Const GBool (BoolV True)) ]

    st0 = empty $ SourcePos path (unsafePos 0) (unsafePos 0)

    _symbolTable = foldl' auxInsert st0 symbols

    auxInsert st (k, e') = insertSymbol k (Entry k GracielaDef e') st
