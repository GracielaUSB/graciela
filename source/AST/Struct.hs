{-# LANGUAGE NamedFieldPuns #-}

module AST.Struct where
--------------------------------------------------------------------------------
import           AST.Declaration  (Declaration)
import           AST.Definition  (Definition)
import qualified AST.Definition  as D
import           AST.Expression  (Expression)
import           AST.Instruction (Instruction)
import qualified AST.Instruction as I
import           AST.Type        (Type)
import           Location
import           SymbolTable
import           Token
import           Treelike

--------------------------------------------------------------------------------
import           Data.List       (intercalate)
import           Data.Monoid     ((<>))
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

data Struct'
  = AbstractDataType
    { atypes :: [Text]
    , decls  :: [Declaration]
    , inv    ::  Expression
    , procs  :: [Definition]
    }
  | DataType
    { abstract ::  Text
    , types    :: [Type]
    , decls    :: [Declaration]
    , repinv   ::  Expression
    , coupinv  ::  Expression
    , procs    :: [Definition]
    }

data Struct
  = Struct
    { structName ::  Text
    , structLoc  ::  Location
    , struct'    ::  Struct'
    }

instance Treelike Struct where
  toTree Struct { structLoc, structName, struct' }
    = case struct' of

      AbstractDataType { atypes, decls, inv, procs } ->
        Node ("Abstract Type " <> unpack structName <> " (" <> intercalate "," (fmap show atypes) <> ") " <> show structLoc)
          [ Node "Declarations" $ fmap toTree decls
          , Node "Invariant" [toTree inv]
          , Node "Procedures" $ fmap toTree procs
          ]
      DataType { abstract, types, decls, repinv, coupinv, procs } ->
        Node ("Abstract Type " <> unpack structName <> " (" <> intercalate "," (fmap show types) <>
              ") implements " <> unpack abstract <> " " <> show structLoc)
          [ Node "Declarations" $ fmap toTree decls
          , Node "Representation Invariant" [toTree repinv]
          , Node "Coupling Invariant" [toTree coupinv]
          , Node "Procedures" $ fmap toTree procs
          ]
    where
