{-# LANGUAGE TemplateHaskell #-}

module Graciela where
--------------------------------------------------------------------------------
import           Contents
import           Data.Monoid
import           Location
import           MyParseError           as P
import           TypeError            as T
import           SymbolTable
import           Text.Parsec
import           Token
import           Type                   (Type(..))

--------------------------------------------------------------------------------
import           Control.Lens           (makeLenses, use, (%=))
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (StateT)
import           Data.Foldable          (toList)
import           Data.Function          (on)
import qualified Data.Map               as Map (empty, fromList, lookup, member, insert)
import           Data.Map               (Map)
import           Data.Sequence          (Seq, (|>))
import qualified Data.Sequence          as Seq (empty, null, sortBy)
import qualified Data.Set               as Set (Set, empty, insert)
import           Data.Text              (Text, pack)
--------------------------------------------------------------------------------


type Graciela a = ParsecT [TokenPos] () (StateT GracielaState Identity) a

data GracielaState = GracielaState
    { _synErrorList    :: Seq MyParseError
    , _symbolTable     :: SymbolTable
    , _sTableErrorList :: Seq TypeError
    , _filesToRead     :: Set.Set String
    , _typesTable      :: Map Text (Type, Location)
    }
    deriving(Show)

makeLenses ''GracielaState

initialTypes :: Map Text (Type, Location)
initialTypes = Map.fromList  
    [ (pack "int",    (GInt,     Location 0 0 "hola"))
    , (pack "float",  (GFloat,   Location 0 0 "hola"))
    , (pack "boolean",(GBoolean, Location 0 0 "hola"))
    , (pack "char",   (GChar,    Location 0 0 "hola"))
    ]


initialState :: GracielaState
initialState = GracielaState
    { _synErrorList    = Seq.empty
    , _symbolTable     = emptyTable
    , _sTableErrorList = Seq.empty
    , _filesToRead     = Set.empty
    , _typesTable      = initialTypes
    }

{- Graciela 2.0-}
typeError :: TypeError -> Graciela ()
typeError err = sTableErrorList %= (|> err)

insertType :: Text -> Type -> Location -> Graciela ()
insertType name t loc = do
    typesTable %= Map.insert name (t, loc)

getType :: Text -> Graciela Type
getType name = do 
    types <- use typesTable
    case Map.lookup name types of 
        Just (t, loc) -> return t 
        Nothing -> return GError
{- Graciela 2.0-}

drawState :: Maybe Int -> GracielaState -> String
drawState n st = if Seq.null $ _synErrorList st
    then if Seq.null $ _sTableErrorList st
        then "\n HUBO UN ERROR PERO LAS LISTAS ESTAN VACIAS... \n"
        else drawError . take' n . Seq.sortBy (compare `on` T.loc) . _sTableErrorList $ st
    else drawError . take' n . Seq.sortBy (compare `on` P.loc) . _synErrorList $ st


drawError list = if Seq.null list
    then "LISTA DE ERRORES VACIA"
    else unlines . map show . toList $ list