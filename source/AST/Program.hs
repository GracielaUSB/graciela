{-# LANGUAGE NamedFieldPuns #-}

module AST.Program where
--------------------------------------------------------------------------------
import           AST.Definition  (Definition)
import qualified AST.Definition  as D
import           AST.Instruction (Instruction)
import qualified AST.Instruction as I
import           Location
import           SymbolTable
import           Token
import           Treelike
import           Type            (Type)
import qualified Type            as T
--------------------------------------------------------------------------------
import           Data.Monoid     ((<>))
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

data Program
  = Program
    { name  :: Text
    , loc   :: Location
    , defs  :: [Definition]
    , insts :: Instruction
    }

instance Treelike Program where
  toTree Program { name, loc, defs, insts } =
    Node ("Program " <> unpack name <> " " <> show loc)
      [ Node "Definitions" (toForest defs)
      , toTree insts
      ]
