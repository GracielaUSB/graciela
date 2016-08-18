{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Graciela where
--------------------------------------------------------------------------------
import           Error
import           Location
import           Parser.Prim
import           SymbolTable
import           Token                     (Token, TokenPos)
import           Type                      (Type (..))
import           TypeError                 as T
--------------------------------------------------------------------------------
import           Control.Applicative       (Alternative)
import           Control.Lens              (makeLenses, use, (%=))
import           Control.Monad             (MonadPlus, void)
import           Control.Monad.State       (MonadState, State)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Foldable             (null, toList)
import           Data.Function             (on)
import qualified Data.List.NonEmpty        as NE
import           Data.Map                  (Map)
import qualified Data.Map                  as Map (empty, fromList, insert,
                                                   lookup, member)
import           Data.Monoid               ((<>))
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq (empty, null, sortBy)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set (empty, insert, singleton)
import           Data.Text                 (Text, pack)
import           Text.Megaparsec           (ParseError (..), ParsecT,
                                            ShowErrorComponent, ShowToken,
                                            getPosition, lookAhead, manyTill,
                                            parseErrorPretty, withRecovery,
                                            (<|>))
import           Text.Megaparsec.Prim      (MonadParsec)
--------------------------------------------------------------------------------

-- type Graciela = ParsecT Error [TokenPos] (State GracielaState)

newtype Graciela a =
  Graciela
    { runGraciela :: ParsecT Error [TokenPos] (State GracielaState) a }
  deriving (Functor, Applicative, Monad, MonadState GracielaState, MonadParsec Error [TokenPos], MonadPlus, Alternative)


data GracielaState = GracielaState
  { _synErrorList :: Seq MyParseError
  , _errors       :: Seq (ParseError TokenPos Error)
  , _symbolTable  :: SymbolTable
  , _filesToRead  :: Set String
  , _currentProc  :: Maybe (Text, SourcePos, [(Text,Type)])
  , _typesTable   :: Map Text (Type, SourcePos)

  , _recSet       :: [Token]
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
  , _filesToRead     = Set.empty
  , _currentProc     = Nothing
  , _typesTable      = initialTypes

  , _recSet          = []
  }

{- Graciela 2.0-}

insertType :: Text -> Type -> SourcePos -> Graciela ()
insertType name t loc =
  typesTable %= Map.insert name (t, loc)

getType :: Text -> Graciela (Maybe Type)
getType name = do
  types <- use typesTable
  case Map.lookup name types of
    Just (t, loc) -> return $ Just t
    Nothing       -> return Nothing


-- putError :: Location -> Error -> Graciela ()
-- putError (Location(from,to)) e = do
--   let err = ParseError (NE.fromList [from]) Set.empty Set.empty (Set.singleton e)
--   errors %= (|> err)

{- Graciela 2.0-}

-- drawState :: Maybe Int -> GracielaState -> String
-- drawState n st = if Seq.null $ _synErrorList st
--   then if Seq.null $ _sTableErrorList st
--     then "\n HUBO UN ERROR PERO LAS LISTAS ESTAN VACIAS... \n"
--     else drawError . take' n . Seq.sortBy (compare `on` T.pos) . _sTableErrorList $ st
--   else drawError . take' n . Seq.sortBy (compare `on` P.loc) . _synErrorList $ st


-- drawError list = if Seq.null list
--   then "LISTA DE ERRORES VACIA"
--   else unlines . map show . toList $ list

-- Provisional
unsafeGenCustomError :: String -> Graciela ()
unsafeGenCustomError msg = Graciela $ do
    pos <- getPosition
    synErrorList %= (|> CustomError msg  (Location (pos,pos)))
