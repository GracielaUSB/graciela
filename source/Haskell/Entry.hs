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
import           Location
import           Treelike
import           AST.Type
--------------------------------------------------------------------------------
import           Control.Lens   (makeLenses)
import           Data.Monoid    ((<>))
import           Data.Sequence  (Seq)
import           Data.Text      (Text, unpack)
--------------------------------------------------------------------------------

data Entry'
  = Var
    { _varType  :: Type
    , _varValue :: Maybe Expression }
  | SelfVar
    { _selfType  :: Type
    , _selfValue :: Maybe Expression }
  | SelfConst
    { _selfType  :: Type
    , _selfConst :: Value }
  | Const
    { _constType  :: Type
    , _constValue :: Value }
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

    Var { _varType, _varValue } ->
      Node ("Variable `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf ("Type: " <> show _varType)
        , case _varValue of
            Nothing     -> leaf "Not initialized"
            Just value  -> Node "Initial value: " [toTree value] ]

    SelfVar { _selfType, _selfValue } ->
      Node ("Self Variable `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf ("Type: " <> show _selfType)
        , case _selfValue of
            Nothing     -> leaf "Not initialized"
            Just value  -> Node "Initial value: " [toTree value] ]

    SelfConst { _selfType, _selfConst } ->
      Node ("Constant `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf $ "Type: " <> show _selfType
        , Node "Value" [toTree _selfConst]]

    Const { _constType, _constValue } ->
      Node ("Constant `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf $  "Type: " <> show _constType
        , Node "Value" [toTree _constValue] ]

    Argument { _argMode, _argType } ->
      Node ("Argument `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf $ "Type: " <> show _argType
        , leaf $ "Mode: " <> show _argMode ]
