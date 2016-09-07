{-# LANGUAGE NamedFieldPuns #-}

module AST.Program where
--------------------------------------------------------------------------------
import           AST.Definition  (Definition)
import qualified AST.Definition  as D
import           AST.Instruction (Instruction)
import qualified AST.Instruction as I
import           AST.Struct      (Struct)
import           Location
import           SymbolTable
import           Token
import           Treelike
--------------------------------------------------------------------------------
import           Data.Foldable   (toList)
import           Data.Monoid     ((<>))
import           Data.Sequence   (Seq)
import           Data.Map        (Map, elems)
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

data Program
  = Program
    { name        :: Text
    , loc         :: Location
    , defs        :: Seq Definition
    , insts       :: Instruction
    , structs     :: Map Text Struct
    , fullStructs :: Map Text Struct
    }

instance Treelike Program where
  toTree Program { name, loc, defs, insts, structs } =
    Node ("Program " <> unpack name <> " " <> show loc)
      [ Node "Structs" (fmap toTree $ elems structs)
      , Node "Definitions" (toForest defs)
      , toTree insts
      ]
