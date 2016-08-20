{-# LANGUAGE NamedFieldPuns #-}

module AST.Object
  ( Object' (..)
  , Object'' (..)
  , notIn
  ) where
--------------------------------------------------------------------------------
import           Location
import           Treelike
import           Type        (Type, ArgMode(..))
--------------------------------------------------------------------------------
import           Data.Monoid ((<>))
import           Data.Text   (Text, unpack)
--------------------------------------------------------------------------------

data Object'' e
  = Variable
    { name :: Text
    , mode :: Maybe ArgMode
    }
  | Member
    { inner :: Object' e
    , field :: Text
    }
  | Index
    { inner :: Object' e
    , index :: e
    }
  | Deref
    { inner :: Object' e
    }
  deriving (Eq)

{- The type variable in `Object' e` allows us to separate this code from
 - the code in `Expression` without creating a cycle.
 -}
data Object' e
  = Object
    { loc     :: Location
    , objType :: Type
    , obj'    :: Object'' e
    }
  | BadObject
    { loc :: Location
    }
  deriving (Eq)

notIn (Object _ _ Variable {mode}) = case mode of
  Just In -> False
  _ -> True
notIn (Object _ _ o) = notIn (inner o)

instance Show e => Show (Object' e) where
  show BadObject {} = ""

  show Object { loc, objType, obj' } = case obj' of
    Variable {name} -> unpack name
    Member {inner,field} -> show inner <> "." <> unpack field
    Index {inner, index} -> show inner <> "[" <> show index <> "]"
    Deref {inner}        -> "*" <> show inner

instance Treelike e => Treelike (Object' e) where
  toTree Object { loc, objType, obj' } = case obj' of

    Variable { name, mode } ->
      leaf $ "Variable "<> argmode <> " `" <> unpack name <> "` " <> show loc
      where argmode = case mode of; Just x -> show x; _ -> ""

    Member { inner, field } ->
      Node ("Member " <> show loc)
        [ toTree inner
        , leaf $ "Field `" <> unpack field <> "`"
        ]

    Index  { inner, index } ->
      Node ("Index " <> show loc)
        [ toTree inner
        , toTree index
        ]

    Deref  { inner } ->
      Node ("Deref " <> show loc)
        [ toTree inner
        ]

  toTree BadObject { loc } =
    leaf $ "Bad Object " <> show loc
