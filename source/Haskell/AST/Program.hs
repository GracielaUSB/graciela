{-# LANGUAGE NamedFieldPuns #-}

module AST.Program where
--------------------------------------------------------------------------------
import           AST.Definition  (Definition)
import           AST.Expression  hiding (loc)
import           AST.Instruction (Instruction)
import           AST.Struct      (Struct)
import           AST.Type
import           Common
import           SymbolTable
import           Token
--------------------------------------------------------------------------------
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (elems, foldrWithKey)
import           Data.Sequence   (Seq)
import           Data.Set        (Set)
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

data Program
  = Program
    { name        :: Text
    , loc         :: Location
    , defs        :: Seq Definition
    , insts       :: Instruction
    , structs     :: Map Text Struct
    , fullStructs :: Map Text (Struct, Set TypeArgs)
    , strings     :: Map Text Int }

instance Treelike Program where
  toTree Program { name, loc, defs, insts, structs, strings } =
    Node ("Program " <> unpack name <> " " <> show loc)
      [ Node "Structs" (toTree <$> Map.elems structs)
      , Node "Definitions" (toForest defs)
      , Node "Strings" $ stringsNode strings
      , toTree insts
      ]

    where
      stringsNode = Map.foldrWithKey aux []
      aux k v f = (: f) (Node (unpack k) [leaf $ show v])
