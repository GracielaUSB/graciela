{-# LANGUAGE NamedFieldPuns #-}

module AST.Struct where
--------------------------------------------------------------------------------
import           AST.Definition  (Definition)
import           AST.Expression  (Expression)
import           AST.Instruction (Instruction)
import           AST.Type        (Type, TypeArgs, fillType)
import           Common
import           SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens    ((%~), _2)
import           Data.List       (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (toList)
import           Data.Sequence   (Seq)
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

type Fields = Map Text (Integer, Type, Bool, Maybe Expression)

data Struct'
  = AbstractDataType
    { inv ::  Expression }
  | DataType
    { abstract      :: Text
    , abstractTypes :: TypeArgs
    , inv           :: Expression
    , repinv        :: Expression
    , coupinv       :: Expression
    , couple        :: Seq Instruction }

data Struct
  = Struct
    { structBaseName :: Text
    , structTypes    :: [Type]
    , structFields   :: Fields
    , structAFields  :: Fields
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
          [ Node "Declarations" $ fields <$> (Map.toList structFields)
          , Node "Invariant" [toTree inv]
          , Node "Procedures" . fmap toTree . toList $ structProcs
          ]
      DataType { abstract, repinv, coupinv, couple } ->
        Node ("Type " <> unpack structBaseName <> " (" <> intercalate "," (fmap show structTypes) <>
              ") implements " <> unpack abstract <> " " <> show structLoc)
          [ Node "Declarations" $ fields <$> (Map.toList structFields)
          , Node "Representation Invariant" [toTree repinv]
          , Node "Couple Invariant" [toTree coupinv]
          , Node "Couple" (toForest couple)
          , Node "Procedures" . fmap toTree . toList $ structProcs
          ]
    where
      fields (name, (_, t, _, maybeExpr)) =
        Node (unpack name) $ [leaf (show t)] <>
          case maybeExpr of
            Just e -> [toTree e]
            _      -> []
