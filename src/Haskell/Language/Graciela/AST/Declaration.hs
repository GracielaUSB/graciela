{-# LANGUAGE NamedFieldPuns #-}

module Language.Graciela.AST.Declaration where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Expression (Expression)
import           Language.Graciela.AST.Type       (Type)
import           Language.Graciela.Common
--------------------------------------------------------------------------------
import           Data.Sequence  (Seq)
import           Data.Text      (Text, unpack)
--------------------------------------------------------------------------------

data Declaration
  = Declaration
    { declLoc  :: Location
    , declType :: Type
    , declIds  :: Seq Text
    }
  | Initialization
    { declLoc   :: Location
    , declType  :: Type
    , declPairs :: Seq (Text, (Expression,Bool)) }

instance Treelike Declaration where
  toTree Declaration { declLoc, declType, declIds } =
    Node ("Declaration" <> show declLoc) $
      leaf ("Type " <> show declType) :
      (fmap (leaf . unpack) . toList $ declIds)

  toTree Initialization { declLoc, declType, declPairs } =
     Node ("Declaration with Initialization" <> show declLoc) $
      leaf ("Type " <> show declType) :
      (fmap pair . toList $ declPairs)

    where
      pair (identifier, (expr,_)) =
        Node (unpack identifier <> " :=")
          [toTree expr]
