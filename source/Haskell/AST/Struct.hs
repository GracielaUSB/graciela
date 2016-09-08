{-# LANGUAGE NamedFieldPuns #-}

module AST.Struct where
--------------------------------------------------------------------------------
import           AST.Declaration (Declaration)
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

--------------------------------------------------------------------------------
import           Data.Foldable   (toList)
import           Data.List       (intercalate)
import           Data.Map.Strict       (Map)
import           Data.Monoid     ((<>))
import           Data.Sequence   (Seq)
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

data Struct'
  = AbstractDataType
    { inv ::  Expression }
  | DataType
    { abstract      ::  Text
    , abstractTypes ::  Map Type Type
    , inv           ::  Expression
    , repinv        ::  Expression
    , coupinv       ::  Expression }

data Struct
  = Struct
    { structName  :: Text
    , structTypes :: [Type]
    , structDecls :: Seq (Int,Declaration)
    , structProcs :: Seq Definition
    , structLoc   :: Location
    , structSt    :: SymbolTable
    , struct'     :: Struct' }

instance Treelike Struct where
  toTree Struct { structLoc, structDecls, structProcs, structTypes, structName, struct' }
    = case struct' of

      AbstractDataType { inv } ->
        Node ("Abstract Type " <> unpack structName <> " (" <> intercalate "," (fmap show structTypes) <> ") " <> show structLoc)
          [ Node "Declarations" $ fmap (toTree . snd) . toList $ structDecls
          , Node "Invariant" [toTree inv]
          , Node "Procedures" $ fmap toTree . toList $ structProcs
          ]
      DataType { abstract, repinv, coupinv } ->
        Node ("Type " <> unpack structName <> " (" <> intercalate "," (fmap show structTypes) <>
              ") implements " <> unpack abstract <> " " <> show structLoc)
          [ Node "Declarations" $ fmap (toTree . snd) . toList $ structDecls
          , Node "Representation Invariant" [toTree repinv]
          , Node "Coupling Invariant" [toTree coupinv]
          , Node "Procedures" $ fmap toTree . toList $ structProcs
          ]
    where
