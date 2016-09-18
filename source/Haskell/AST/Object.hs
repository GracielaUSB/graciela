{-# LANGUAGE NamedFieldPuns #-}

module AST.Object
  ( Object' (..)
  , Object'' (..)
  , objMode
  , notIn
  ) where
--------------------------------------------------------------------------------
import           AST.Type      (ArgMode (..), Type', TypeArgs')
import           Location
import           Treelike
--------------------------------------------------------------------------------
import           Data.Foldable (toList)
import           Data.Monoid   ((<>))
import           Data.Sequence (Seq)
import           Data.Text     (Text, unpack)
--------------------------------------------------------------------------------

data Object'' e
  = Variable
    { name :: Text
    , mode :: Maybe ArgMode }
  | Member
    { inner     :: Object' e
    , field     :: Integer
    , fieldName :: Text  }
  | Index
    { inner   :: Object' e
    , indices :: Seq e }
  | Deref
    { inner :: Object' e }
  deriving (Eq)

{- The type variable in `Object' e` allows us to separate this code from
 - the code in `Expression` without creating a cycle. -}
data Object' e
  = Object
    { loc     :: Location
    , objType :: Type' e
    , obj'    :: Object'' e }
  deriving (Eq)

objMode (Object _ _ Variable {mode}) = mode
objMode (Object _ _ o) = objMode (inner o)

notIn obj = objMode obj /= Just In

instance Show e => Show (Object' e) where
  show Object { loc, objType, obj' } = case obj' of
    Variable {name} -> unpack name
    Member {inner, fieldName} -> show inner <> "." <> unpack fieldName
    Index { inner, indices } -> show inner <> show (toList indices)
    Deref {inner}        -> "*" <> show inner

instance Treelike e => Treelike (Object' e) where
  toTree Object { loc, objType, obj' } = case obj' of

    Variable { name, mode } ->
      leaf $ "Variable "<> argmode <> " `" <> unpack name <> "` " <> show loc
      where argmode = case mode of; Just x -> show x; _ -> ""

    Member { inner, fieldName } ->
      Node ("Member " <> show loc)
        [ toTree inner
        , leaf $ "Field `" <> show fieldName <> "`"
        ]

    Index  { inner, indices } ->
      Node ("Index " <> show loc)
        [ toTree inner
        , Node "indices" $ toForest indices
        ]

    Deref  { inner } ->
      Node ("Deref " <> show loc)
        [ toTree inner
        ]
