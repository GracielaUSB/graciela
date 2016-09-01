{-# LANGUAGE NamedFieldPuns #-}

module AST.Definition where
--------------------------------------------------------------------------------
import           AST.Declaration (Declaration)
import           AST.Expression  (Expression)
import qualified AST.Expression  as E
import           AST.Instruction (Instruction)
import qualified AST.Instruction as I
import           Location
import           SymbolTable
import           Treelike
import           Type            (ArgMode, Type)
--------------------------------------------------------------------------------
import           Data.Foldable   (toList)
import           Data.Monoid     ((<>))
import           Data.Sequence   (Seq)
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

data Definition'
  = FunctionDef
    { funcBody    :: Expression
    , funcParams  :: Seq (Text, Type)
    , funcRetType :: Type }
  | ProcedureDef
    { procDecl   :: Seq (Either Declaration Instruction)
    , procBody   :: Instruction
    , procParams :: Seq (Text, Type, ArgMode) }
  | AbstractProcedureDef
    { abstParams :: Seq (Text, Type, ArgMode) }

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
      FunctionDef { funcBody, funcRetType, funcParams } ->
        Node ("Function " <> unpack defName <> " -> " <> show funcRetType <> " " <> show defLoc)
          [ Node "Parameters" (showFPs funcParams)
          , boundNode
          , Node "Body" [toTree funcBody]
          ]

      ProcedureDef { procDecl, procBody, procParams } ->
        Node ("Procedure " <> unpack defName <> " " <> show defLoc)
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

    where
      showPs :: Seq (Text, Type, ArgMode) -> [Tree String]
      showPs = fmap (\(n,t,m) -> leaf (show m <> " " <> unpack n <> " : " <> show t)) . toList
      showFPs :: Seq (Text, Type) -> [Tree String]
      showFPs = fmap (\(n,t) -> leaf (unpack n <> " : " <> show t)) . toList
      boundNode = case bound of
        Just b -> Node "Bound" [toTree b]
        Nothing -> leaf "Not bounded"
