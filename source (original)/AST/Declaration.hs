{-# LANGUAGE NamedFieldPuns #-}

module AST.Declaration where
--------------------------------------------------------------------------------
import           AST.Expression (Expression, Object)
import qualified AST.Expression as E
import           Type
import           Location
import           SymbolTable
import           Token
import           Treelike
--------------------------------------------------------------------------------
import           Data.Monoid    ((<>))
import           Data.Text      (Text, unpack)
import Data.Sequence (Seq)
import Data.Foldable (toList)
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


getFields :: Declaration -> [Type]
getFields Declaration {declType, declIds} =
  toList . fmap (const declType) $ declIds

getFields Initialization {declType, declPairs} =
  toList . fmap (const declType) $ declPairs

getFields BadDeclaration{} = [] 


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
