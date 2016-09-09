{-# LANGUAGE NamedFieldPuns #-}

module AST.Struct where
--------------------------------------------------------------------------------
import           AST.Declaration (Declaration)
import           AST.Definition  (Definition)
import qualified AST.Definition  as D
import           AST.Expression  (Expression)
import           AST.Instruction (Instruction)
import qualified AST.Instruction as I
import           AST.Type        (Type (GTypeVar), TypeArgs)
import           Location
import           SymbolTable
import           Token
import           Treelike
--------------------------------------------------------------------------------
import           Data.Array      ((!))
import           Data.Foldable   (toList)
import           Data.List       (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (lookup)
import           Data.Monoid     ((<>))
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
    , coupinv       :: Expression }

data Struct
  = Struct
    { structName   :: Text
    , structTypes  :: [Type]
    , structFields :: Fields
    , structProcs  :: Seq Definition
    , structLoc    :: Location
    , structSt     :: SymbolTable
    , struct'      :: Struct' }


fillTypes :: TypeArgs -> Fields -> Fields
fillTypes typeArgs fields = f <$> fields
  where
    f (_a, GTypeVar i, _c) = (_a, typeArgs ! i, _c)
    f tuple = tuple


instance Treelike Struct where
  toTree Struct { structLoc, structFields, structProcs, structTypes, structName, struct' }
    = case struct' of

      AbstractDataType { inv } ->
        Node ("Abstract Type " <> unpack structName <> " (" <> intercalate "," (fmap show structTypes) <> ") " <> show structLoc)
          [ {-Node "Declarations" $ fmap (toTree . snd) . toList $ structFields TODO
          ,-} Node "Invariant" [toTree inv]
          , Node "Procedures" . fmap toTree . toList $ structProcs
          ]
      DataType { abstract, repinv, coupinv } ->
        Node ("Type " <> unpack structName <> " (" <> intercalate "," (fmap show structTypes) <>
              ") implements " <> unpack abstract <> " " <> show structLoc)
          [ {-Node "Declarations" $ fmap (toTree . snd) . toList $ structFields TODO
          ,-} Node "Representation Invariant" [toTree repinv]
          , Node "Coupling Invariant" [toTree coupinv]
          , Node "Procedures" . fmap toTree . toList $ structProcs
          ]
