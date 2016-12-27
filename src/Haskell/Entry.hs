{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Entry
  ( Entry (..)
  , Entry' (..)
  -- , Value (..)
  , info
  , varType
  ) where
--------------------------------------------------------------------------------
import           AST.Expression
import           AST.Type
import           Common
import           Location
import           Treelike
--------------------------------------------------------------------------------
import           Control.Lens   (makeLenses)
import           Data.Sequence  (Seq)
import           Data.Text      (Text, unpack)
--------------------------------------------------------------------------------

data Entry'
  = Var
    { _varType  :: Type
    , _varValue :: Maybe Expression
    , _varConst :: Bool }
  | SelfVar -- Variables declared inside of a Data Type. these variables are only used inside invariants
    { _selfType  :: Type
    , _selfValue :: Maybe Expression
    , _selfConst :: Bool }
  | Alias
    { _aliasType  :: Type
    , _aliasValue :: Value }
  | Argument
    { _argMode :: ArgMode
    , _argType :: Type }
  deriving (Eq)

makeLenses ''Entry'


data Entry
  = Entry
    { _entryName :: Text
    , _loc       :: Location
    , _info      :: Entry' }

makeLenses ''Entry


instance Treelike Entry where
  toTree Entry { _entryName, _loc, _info } = case _info of

    Var { _varType, _varValue, _varConst } ->
      Node ((if _varConst then "Constant" else "Variable") <> " `" <>
        unpack _entryName <> "` " <> show _loc)
        [ leaf ("Type: " <> show _varType)
        , case _varValue of
            Nothing    -> leaf "Not initialized"
            Just value -> Node "Value: " [toTree value] ]

    SelfVar { _selfType, _selfValue } ->
      Node ("Self Variable `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf ("Type: " <> show _selfType)
        , case _selfValue of
            Nothing    -> leaf "Not initialized"
            Just value -> Node "Initial value: " [toTree value] ]
    Alias { _aliasType, _aliasValue } ->
      Node ("Alias")
        [ leaf ("Type: " <> show _aliasType)
        , leaf ("Value: " <> show _aliasValue)]
    Argument { _argMode, _argType } ->
      Node ("Argument `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf $ "Type: " <> show _argType
        , leaf $ "Mode: " <> show _argMode ]
