{-# LANGUAGE NamedFieldPuns #-}

module AST.Struct where
--------------------------------------------------------------------------------
import           AST.Definition  (Definition)
import qualified AST.Definition  as D
import           AST.Expression  (Expression)
import           AST.Instruction (Instruction)
import qualified AST.Instruction as I
import           Location
import           SymbolTable
import           Token
import           Treelike
import           Type            (Type)
import qualified Type            as T
--------------------------------------------------------------------------------
import           Data.List       (intercalate)
import           Data.Monoid     ((<>))
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

data Struct'
  = AbstractDataType
    { atypes :: [Text]
    , decls  :: [Instruction]
    , inv    ::  Expression
    , procs  :: [Definition]
    }
  | DataType
    { abstract ::  Text
    , types    :: [Either Text Type]
    , decls    :: [Instruction]
    , repinv   ::  Expression
    , coupinv  ::  Expression
    , procs    :: [Definition]
    }

data Struct
  = Struct
    { name    ::  Text
    , loc     ::  Location

    , struct' ::  Struct'
    }

instance Treelike Struct where
  toTree Struct { loc, name, struct' }
    = case struct' of

      AbstractDataType { atypes, decls, inv, procs } ->
        Node ("Abstract Type " <> unpack name <> " (" <> intercalate "," (fmap show atypes) <> ") " <> show loc)
          [ Node "Declarations" (fmap toTree decls)
          , Node "Invariant" [toTree inv]
          , Node "Procedures" (fmap toTree procs)
          ]
      DataType { abstract, types, decls, repinv, coupinv, procs } ->
        Node ("Abstract Type " <> unpack name <> " (" <> intercalate "," (fmap showType types) <>
              ") implements " <> unpack abstract <> " " <> show loc)
          [ Node "Declarations" (fmap toTree decls)
          , Node "Representation Invariant" [toTree repinv]
          , Node "Coupling Invariant" [toTree coupinv]
          , Node "Procedures" (fmap toTree procs)
          ]


    where
      showType (Left text) = show text
      showType (Right t)   = show t
