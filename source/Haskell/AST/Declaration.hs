{-# LANGUAGE NamedFieldPuns #-}

module AST.Declaration where
--------------------------------------------------------------------------------
import           AST.Expression
import           AST.Type       (Expression, Object, Type)
import           Location
import           SymbolTable
import           Token
import           Treelike
--------------------------------------------------------------------------------
import           Data.Foldable  (toList)
import           Data.Semigroup ((<>))
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
