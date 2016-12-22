{-# LANGUAGE NamedFieldPuns #-}

module AST.Object
  ( Object (..)
  , Object' (..)
  ) where
--------------------------------------------------------------------------------
import {-# SOURCE #-} AST.Type (Type, ArgMode)
import {-# SOURCE #-} AST.Expression (Expression)
--------------------------------------------------------------------------------
import           Common
--------------------------------------------------------------------------------
import           Data.Sequence (Seq)
import           Data.Text     (Text)
--------------------------------------------------------------------------------

data Object'
  = Variable
    { name :: Text
    , mode :: Maybe ArgMode }
  | Member
    { inner     :: Object
    , field     :: Integer
    , fieldName :: Text  }
  | Index
    { inner   :: Object
    , indices :: Seq Expression }
  | Deref
    { inner :: Object }
  deriving (Eq)

data Object
  = Object
    { loc     :: Location
    , objType :: Type
    , obj'    :: Object' }
  deriving (Eq)


instance Show Object where
  show Object { obj' } = case obj' of
    Variable {name}           -> unpack name
    Member {inner, fieldName} -> show inner <> "." <> unpack fieldName
    Index { inner, indices }  -> show inner <> show (toList indices)
    Deref {inner}             -> "*" <> show inner

instance Treelike Object where
  toTree Object { loc, obj', objType } = case obj' of

    Variable { name, mode } ->
      leaf $ "Variable "<> argmode <> " ("<> show objType<>") `" <> unpack name <> "` " <> show loc
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
