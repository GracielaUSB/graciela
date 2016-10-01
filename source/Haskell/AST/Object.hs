{-# LANGUAGE NamedFieldPuns #-}

module AST.Object
  ( Object' (..)
  , Object'' (..)
  ) where
--------------------------------------------------------------------------------
-- import           AST.Type      (ArgMode (..), Type', TypeArgs')
import           Location
import           Treelike
--------------------------------------------------------------------------------
import           Data.Foldable (toList)
import           Data.Semigroup ((<>))
import           Data.Sequence (Seq)
import           Data.Text     (Text, unpack)
--------------------------------------------------------------------------------

data Object'' t m e
  = Variable
    { name :: Text
    , mode :: Maybe m }
  | Member
    { inner     :: Object' t m e
    , field     :: Integer
    , fieldName :: Text  }
  | Index
    { inner   :: Object' t m e
    , indices :: Seq e }
  | Deref
    { inner :: Object' t m e }
  deriving (Eq)

{- The type variable in `Object' e` allows us to separate this code from
 - the code in `Expression` without creating a cycle. -}
data Object' t m e
  = Object
    { loc     :: Location
    , objType :: t
    , obj'    :: Object'' t m e }
  deriving (Eq)

instance Show e => Show (Object' t m e) where
  show Object { loc, objType, obj' } = case obj' of
    Variable {name} -> unpack name
    Member {inner, fieldName} -> show inner <> "." <> unpack fieldName
    Index { inner, indices } -> show inner <> show (toList indices)
    Deref {inner}        -> "*" <> show inner

instance (Show m, Treelike e) => Treelike (Object' t m e) where
  toTree Object { loc, objType, obj' } = case obj' of

    Variable { name, mode } ->
      leaf $ "Variable "<> argmode <> " `" <> unpack name <> "` " <> show loc
      where argmode = case mode of; Just x -> show x; _ -> ""

    Member { inner, fieldName } ->
      Node ("Member " <> show loc)
        [ toTree inner
        , leaf $ "Field `" <> show fieldName <> "`" ]

    Index  { inner, indices } ->
      Node ("Index " <> show loc)
        [ toTree inner
        , Node "indices" $ toForest indices ]

    Deref  { inner } ->
      Node ("Deref " <> show loc)
        [ toTree inner ]
