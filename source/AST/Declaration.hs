{-# LANGUAGE NamedFieldPuns #-}

module AST.Declaration where
--------------------------------------------------------------------------------
import           AST.Expression (Expression, Object)
import qualified AST.Expression as E
import           AST.Type
import           Location
import           SymbolTable
import           Token
import           Treelike
--------------------------------------------------------------------------------
import           Data.Monoid    ((<>))
import           Data.Text      (Text, unpack)

data Declaration
  = Declaration
    { declLoc   :: Location
    , declType  ::  Type
    , declLvals :: [Text]
    , declExprs :: [Expression]
    }
  | BadDeclaration
    { declLoc :: Location
    }


instance Treelike Declaration where
  toTree BadDeclaration { declLoc } = 
    leaf ("BadDeclaration" <> show declLoc)

  toTree Declaration {declLoc, declType, declLvals, declExprs} =
     Node ("Declaration" <> show declLoc) $
      leaf ("Type " <> show declType) :
      case declExprs of
        [] -> fmap (\id -> Node (unpack id) [leaf "Value: None"]) declLvals
        _  -> zipWith declarationToTree declLvals declExprs
  
    where
      declarationToTree ident expr = Node "(:=)" 
          [ leaf  ("`" <> unpack ident <> "`")
          , toTree expr 
          ]
