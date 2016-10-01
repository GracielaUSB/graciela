{-# LANGUAGE NamedFieldPuns #-}

module AST.Struct where
--------------------------------------------------------------------------------
import           AST.Declaration (Declaration)
import           AST.Definition  (Definition)
import           AST.Instruction (Instruction)
import           AST.Type        (Expression, Type (GTypeVar), TypeArgs,
                                  fillType)
import           Location
import           SymbolTable
import           Token
import           Treelike
--------------------------------------------------------------------------------
import           Control.Lens    ((%~), _2)
import           Data.Foldable   (toList)
import           Data.List       (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (lookup)
import           Data.Semigroup ((<>))
import           Data.Sequence   (Seq)
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

type Fields = Map Text (Integer, Type, Maybe Expression)

data Struct'
  = AbstractDataType
    { inv ::  Expression }
  | DataType
    { abstract      :: Text
    , abstractTypes :: TypeArgs
    , inv           :: Expression
    , repinv        :: Expression
    , couple        :: Seq Instruction }

data Struct
  = Struct
    { structBaseName :: Text
    , structTypes    :: [Type]
    , structFields   :: Fields
    , structProcs    :: Map Text Definition
    , structLoc      :: Location
    , structSt       :: SymbolTable
    , struct'        :: Struct' }


fillTypes :: TypeArgs -> Fields -> Fields
fillTypes typeArgs fields = (_2 %~ fillType typeArgs) <$> fields


instance Treelike Struct where
  toTree Struct { structLoc, structFields, structProcs, structTypes, structBaseName, struct' }
    = case struct' of

      AbstractDataType { inv } ->
        Node ("Abstract Type " <> unpack structBaseName <> " (" <> intercalate "," (fmap show structTypes) <> ") " <> show structLoc)
          [ {-Node "Declarations" $ fmap (toTree . snd) . toList $ structFields TODO
          ,-} Node "Invariant" [toTree inv]
          , Node "Procedures" . fmap toTree . toList $ structProcs
          ]
      DataType { abstract, repinv, couple } ->
        Node ("Type " <> unpack structBaseName <> " (" <> intercalate "," (fmap show structTypes) <>
              ") implements " <> unpack abstract <> " " <> show structLoc)
          [ {-Node "Declarations" $ fmap (toTree . snd) . toList $ structFields TODO
          ,-} Node "Representation Invariant" [toTree repinv]
          , Node "Couple" (toForest couple)
          , Node "Procedures" . fmap toTree . toList $ structProcs
          ]
