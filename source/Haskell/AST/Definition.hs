{-# LANGUAGE NamedFieldPuns #-}

module AST.Definition where
--------------------------------------------------------------------------------
import           AST.Declaration (Declaration)
import           AST.Expression  (Expression)
import qualified AST.Expression  as E
import           AST.Instruction (Instruction (..), Instruction' (..))
import qualified AST.Instruction as I
import           Location
import           SymbolTable
import           Treelike
import           AST.Type            (ArgMode (..), Type (..))
--------------------------------------------------------------------------------
import           Data.Foldable   (toList)
import           Data.Monoid     ((<>))
import           Data.Sequence   (Seq)
import           Data.Sequence   as Seq (zip)
import           Data.Text       (Text, pack, unpack)
--------------------------------------------------------------------------------

data Definition'
  = FunctionDef
    { funcBody      :: Expression
    , funcParams    :: Seq (Text, Type)
    , funcRetType   :: Type
    , funcRecursive :: Bool }
  | ProcedureDef
    { procDecl      :: Seq (Either Declaration Instruction)
    , procBody      :: Instruction
    , procParams    :: Seq (Text, Type, ArgMode)
    , procRecursive :: Bool }
  | AbstractProcedureDef
    { abstParams :: Seq (Text, Type, ArgMode) }
  | AbstractFunctionDef
    { abstFParams  :: Seq (Text, Type)
    , funcRetType :: Type }

data Definition
  = Definition
    { defLoc  :: Location
    , defName :: Text
    , pre     :: Expression
    , post    :: Expression
    , bound   :: Maybe Expression
    , def'    :: Definition' }


instance Treelike Definition where
  toTree Definition { defLoc, defName, pre, post, bound, def' }
    = case def' of
      FunctionDef { funcBody, funcRetType, funcParams, funcRecursive } ->
        let rec = if funcRecursive then "Recursive " else ""
        in Node (rec <> "Function " <> unpack defName <> " -> " <> show funcRetType <> " " <> show defLoc)
          [ Node "Parameters" (showFPs funcParams)
          , boundNode
          , Node "Body" [toTree funcBody]
          ]

      ProcedureDef { procDecl, procBody, procParams, procRecursive} ->
        let rec = if procRecursive then "Recursive " else ""
        in Node (rec <> "Procedure " <> unpack defName <> " " <> show defLoc)
          [ Node "Parameters" (showPs procParams)
          , Node "Declarations" $
              (\x -> case x of; Left a -> toTree a; Right b -> toTree b) <$> toList procDecl
          , Node "Precondition" [toTree pre]
          , boundNode
          , Node "Body" [toTree procBody]
          , Node "Postcondition" [toTree post]
          ]

      AbstractProcedureDef {abstParams} ->
        Node ("Abstarct Procedure " <> unpack defName <> " " <> show defLoc)
          [ Node "Parameters" (showPs abstParams)
          , Node "Precondition" [toTree pre]
          , Node "Postcondition" [toTree post]
          ]

      AbstractFunctionDef {abstFParams, funcRetType} ->
        Node ("Abstarct Function " <> unpack defName <> " -> " <>
               show funcRetType <> " " <> show defLoc)
          [ Node "Parameters" (showFPs abstFParams)
          , Node "Precondition" [toTree pre]
          , Node "Postcondition" [toTree post]
          ]

    where
      showPs :: Seq (Text, Type, ArgMode) -> [Tree String]
      showPs = fmap (\(n,t,m) -> leaf (show m <> " " <> unpack n <> " : " <> show t)) . toList
      showFPs :: Seq (Text, Type) -> [Tree String]
      showFPs = fmap (\(n,t) -> leaf (unpack n <> " : " <> show t)) . toList
      boundNode = case bound of
        Just b -> Node "Bound" [toTree b]
        Nothing -> leaf "Not bounded"
