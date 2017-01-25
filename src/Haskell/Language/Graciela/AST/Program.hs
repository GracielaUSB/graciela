{-# LANGUAGE NamedFieldPuns #-}

module Language.Graciela.AST.Program where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Definition  (Definition)
import           Language.Graciela.AST.Expression  hiding (loc)
import           Language.Graciela.AST.Instruction (Instruction)
import           Language.Graciela.AST.Struct      (Struct)
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.SymbolTable
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map (elems, foldrWithKey)
import           Data.Sequence                     (Seq)
import           Data.Set                          (Set)
import           Data.Text                         (Text, unpack)
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
