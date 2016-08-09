{-# LANGUAGE TemplateHaskell #-}

module Graciela where
--------------------------------------------------------------------------------
import           MyParseError           as P
import           Parser.Prim
import           Location
import           SymbolTable
import           Token
import           Type                   (Type (..))
import           TypeError              as T
--------------------------------------------------------------------------------
import           Control.Lens           (makeLenses, use, (%=))
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (State)
import qualified Data.List.NonEmpty     as NE
import           Data.Foldable          (toList, null)
import           Data.Function          (on)
import           Data.Map               (Map)
import qualified Data.Map               as Map (empty, fromList, insert, lookup,
                                                member)
import           Data.Monoid            ((<>))
import           Data.Sequence          (Seq, (|>))
import qualified Data.Sequence          as Seq (empty, null, sortBy)
import qualified Data.Set               as Set (Set, empty, insert, singleton)
import           Data.Text              (Text, pack)
import           Text.Megaparsec        (ParseError(..), ParsecT, getPosition,
                                         ShowToken, ShowErrorComponent,parseErrorPretty)
--------------------------------------------------------------------------------

type Graciela = ParsecT Error [TokenPos] (State GracielaState)

data GracielaState = GracielaState
  { _synErrorList    :: Seq MyParseError
  , _errors          :: Seq (ParseError TokenPos Error)
  , _symbolTable     :: SymbolTable
  , _sTableErrorList :: Seq TypeError
  , _filesToRead     :: Set.Set String
  , _currentProc     :: Maybe (Text, SourcePos, [(Text,Type)])
  , _typesTable      :: Map Text (Type, SourcePos)
  }

makeLenses ''GracielaState


gracielaDef :: SourcePos
gracielaDef = SourcePos "graciela.def" (unsafePos 1) (unsafePos 1)


initialTypes :: Map Text (Type, SourcePos)
initialTypes = Map.fromList
  [ (pack "int",    (GInt,   gracielaDef))
  , (pack "float",  (GFloat, gracielaDef))
  , (pack "boolean",(GBool,  gracielaDef))
  , (pack "char",   (GChar,  gracielaDef))
  ]


initialState :: GracielaState
initialState = GracielaState
  { _synErrorList    = Seq.empty
  , _errors          = Seq.empty
  , _symbolTable     = empty gracielaDef
  , _sTableErrorList = Seq.empty
  , _filesToRead     = Set.empty
  , _currentProc     = Nothing
  , _typesTable      = initialTypes
  }

{- Graciela 2.0-}
typeError :: TypeError -> Graciela ()
typeError err = sTableErrorList %= (|> err)

insertType :: Text -> Type -> SourcePos -> Graciela ()
insertType name t loc =
  typesTable %= Map.insert name (t, loc)

getType :: Text -> Graciela Type
getType name = do
  types <- use typesTable
  case Map.lookup name types of
    Just (t, loc) -> return t
    Nothing       -> return GError
{- Graciela 2.0-}

drawState :: Maybe Int -> GracielaState -> String
drawState n st = if Seq.null $ _synErrorList st
  then if Seq.null $ _sTableErrorList st
    then "\n HUBO UN ERROR PERO LAS LISTAS ESTAN VACIAS... \n"
    else drawError . take' n . Seq.sortBy (compare `on` T.pos) . _sTableErrorList $ st
  else drawError . take' n . Seq.sortBy (compare `on` P.loc) . _synErrorList $ st


drawError list = if Seq.null list
  then "LISTA DE ERRORES VACIA"
  else unlines . map show . toList $ list


syntaxError :: MyParseError -> Graciela ()
syntaxError err =
  synErrorList %= (|> err)



-- Provisional
genCustomError :: String -> Graciela ()
genCustomError msg = do
    pos <- getPosition
    synErrorList %= (|> CustomError msg  (Location (pos,pos)))

putError :: Location -> Error -> Graciela ()
putError (Location(from,to)) e = do 
  let err = ParseError (NE.fromList [from]) Set.empty Set.empty (Set.singleton e)
  errors %= (|> err)
