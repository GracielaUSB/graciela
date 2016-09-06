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
    }
  | Initialization
    { declLoc   :: Location
    , declType  :: Type
    , declPairs :: Seq (Text, Expression) }

getFields :: Declaration -> [Type]
getFields Declaration {declType, declIds} =
  toList . fmap (const declType) $ declIds

getFields Initialization {declType, declPairs} =
  toList . fmap (const declType) $ declPairs


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
      pair (identifier, expr) =
        Node (unpack identifier <> " :=")
          [toTree expr]
