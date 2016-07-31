{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Entry
  ( Entry'(..)
  , Entry''(..)
  ) where
--------------------------------------------------------------------------------
import           Location
import           Treelike
import           Type
--------------------------------------------------------------------------------
import           Control.Lens (makeLenses)
import           Data.Monoid  ((<>))
import           Data.Text    (Text, unpack)
--------------------------------------------------------------------------------

data Value = I Integer | C Char | D Double {-| S String-} | B Bool
  deriving (Eq)

instance Show Value where
  show (I i) = show i
  show (C c) = show c
  show (D d) = show d
  -- show (S s) = show s
  show (B b) = show b


data Entry'' s
  = Variable
    { _varType  :: Type
    , _varValue :: Maybe Value
    }
  | Constant
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
    , _posFrom   :: !SourcePos
    , _posTo     :: !SourcePos
    , _info      :: Entry'' s
    }

makeLenses ''Entry'


instance Treelike (Entry' s) where
  toTree Entry { _entryName, _posFrom, _posTo, _info } = case _info of

    Variable { _varType, _varValue } ->
      Node ("Variable `" <> unpack _entryName <> "`" <> posFromTo)
        [ leaf ("Type: " <> show _varType)
        , leaf $ case _varValue of
            Just x -> "Initial value: " <> show x
            _      -> "Not initialized"
        ]

    Constant { _constType, _constValue } ->
      Node ("Constant `" <> unpack _entryName <> "`" <> posFromTo)
        [ leaf $ "Type: " <> show _constType
        , leaf $ "Value: " <> show _constValue
        ]

    Argument { _argMode, _argType } ->
      Node ("Argument `" <> unpack _entryName ++ "`" <> posFromTo)
        [ leaf $ "Type: " <> show _argType
        , leaf $ "Mode: " <> show _argMode
        ]

    Function { _funcType, _funcArgs, _funcTable } ->
      Node ("Function `" <> unpack _entryName ++ "`" <> posFromTo)
        [ leaf $ show _funcType
        ]

    Procedure { _procType, _procArgs, _procTable } ->
      Node ("Procedure `" <> unpack _entryName ++ "`" <> posFromTo)
        [ leaf $ show _procType
        ]

    AbstractTypeEntry {} ->
      leaf ("Abstract Data Type `" <> unpack _entryName ++ "`" <> posFromTo)

    TypeEntry {} ->
      leaf ("Data Type `" <> unpack _entryName ++ "`" <> posFromTo)

    where
      posFromTo = " (" ++ showPos' _posFrom ++ " - " ++ showPos' _posTo ++ ")"
