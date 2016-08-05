{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Entry
  ( Entry' (..)
  , Entry'' (..)
  , Value (..)
  , info
  , varType
  ) where
--------------------------------------------------------------------------------
import           Location
import           Treelike
import           AST.Expression
import           Type
--------------------------------------------------------------------------------
import           Control.Lens (makeLenses)
import           Data.Monoid  ((<>))
import           Data.Text    (Text, unpack)
--------------------------------------------------------------------------------

data Value = I Integer | C Char | F Double {-| S String-} | B Bool | None
  deriving (Eq)

instance Show Value where
  show (I i) = show i
  show (C c) = show c
  show (F f) = show f
  show None  = "None"
  show (B b) = show b


data Entry'' s
  = Var
    { _varType  :: Type
    , _varValue :: Value
    }
  | Const
    { _constType  :: Type
    , _constValue :: Value
    }
  | Argument
    { _argMode :: ArgMode
    , _argType :: Type
    }
  | Function
    { _funcType  :: Type
    , _funcArgs  :: [Text]
    , _funcTable :: s
    }
  | Procedure
    { _procType  :: Type
    , _procArgs  :: [Text]
    , _procTable :: s
    }
  | AbstractTypeEntry
  | TypeEntry
  deriving (Eq)

makeLenses ''Entry''


data Entry' s
  = Entry
    { _entryName :: Text
    , _loc       :: Location
    , _info      :: Entry'' s
    }

makeLenses ''Entry'


instance Treelike (Entry' s) where
  toTree Entry { _entryName, _loc, _info } = case _info of

    Var { _varType, _varValue } ->
      Node ("Variable `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf ("Type: " <> show _varType)
        , leaf $ case _varValue of
            None -> "Not initialized"
            _    -> "Initial value: " <> show _varValue
        ]

    Const { _constType, _constValue } ->
      Node ("Constant `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf $ "Type: " <> show _constType
        , leaf $ "Value: " <> show _constValue
        ]

    Argument { _argMode, _argType } ->
      Node ("Argument `" <> unpack _entryName ++ "` " <> show _loc)
        [ leaf $ "Type: " <> show _argType
        , leaf $ "Mode: " <> show _argMode
        ]

    Function { _funcType, _funcArgs, _funcTable } ->
      Node ("Function `" <> unpack _entryName ++ "` " <> show _loc)
        [ leaf $ show _funcType
        ]

    Procedure { _procType, _procArgs, _procTable } ->
      Node ("Procedure `" <> unpack _entryName ++ "` " <> show _loc)
        [ leaf $ show _procType
        ]

    AbstractTypeEntry {} ->
      leaf ("Abstract Data Type `" <> unpack _entryName ++ "` " <> show _loc)

    TypeEntry {} ->
      leaf ("Data Type `" <> unpack _entryName ++ "` " <> show _loc)
