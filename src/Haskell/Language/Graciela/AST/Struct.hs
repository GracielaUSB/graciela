{-# LANGUAGE NamedFieldPuns #-}

module Language.Graciela.AST.Struct where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Definition  (Definition)
import           Language.Graciela.AST.Expression  (Expression)
import           Language.Graciela.AST.Instruction (Instruction)
import           Language.Graciela.AST.Type        (Type, TypeArgs, fillType)
import           Language.Graciela.Common
import           Language.Graciela.SymbolTable
--------------------------------------------------------------------------------
import           Control.Lens                      ((%~), _2)
import           Data.List                         (intercalate)
import qualified Data.Map.Strict                   as Map (toList)
import           Data.Text                         (Text)
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
    , structFields   :: Fields -- All struct field
    , structAFields  :: Fields -- structFields - abstract fields
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
