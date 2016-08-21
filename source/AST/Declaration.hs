{-# LANGUAGE NamedFieldPuns #-}

module AST.Declaration where
--------------------------------------------------------------------------------
import           AST.Expression (Expression, Object)
import qualified AST.Expression as E
import           Location
import           SymbolTable
import           Token
import           Treelike
import           Type
--------------------------------------------------------------------------------
import           Data.Foldable  (toList)
import           Data.Monoid    ((<>))
import           Data.Sequence  (Seq)
import           Data.Text      (Text, unpack)
--------------------------------------------------------------------------------

data Declaration
  = Declaration
    { declLoc  :: Location
    , declType :: Type
    , declIds  :: Seq Text
    -- , declExprs :: [Expression]
    }
  | Initialization
    { declLoc   :: Location
    , declType  :: Type
    , declPairs :: Seq (Text, Expression) }
  | BadDeclaration
    { declLoc :: Location
    }


instance Treelike Declaration where
  toTree BadDeclaration { declLoc } =
    leaf ("BadDeclaration" <> show declLoc)

  toTree Declaration { declLoc, declType, declIds } =
    Node ("Declaration" <> show declLoc) $
      leaf ("Type " <> show declType) :
      (map (leaf . unpack) . toList $ declIds)

  toTree Initialization { declLoc, declType, declPairs } =
     Node ("Declaration with Initialization" <> show declLoc) $
      leaf ("Type " <> show declType) :
      (map pair . toList $ declPairs)

    where
      pair (identifier, expr) =
        Node (unpack identifier <> " :=")
          [toTree expr]
