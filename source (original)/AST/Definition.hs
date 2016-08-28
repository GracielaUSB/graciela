{-# LANGUAGE NamedFieldPuns #-}

module AST.Definition where
--------------------------------------------------------------------------------
import           AST.Declaration  (Declaration)
import           AST.Expression  (Expression)
import qualified AST.Expression  as E
import           AST.Instruction (Instruction)
import qualified AST.Instruction as I
import           Type        (Type, ArgMode)
import           Location
import           SymbolTable
import           Treelike
--------------------------------------------------------------------------------
import           Data.Monoid     ((<>))
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

data Definition'
  = FunctionDef
    { funcBody :: Expression
    , retType  :: Type
    , fparams   :: [(Text, Type)]
    }
  | ProcedureDef
    { procDecl :: [Either Declaration Instruction]
    , params   :: [(Text, Type, ArgMode)]
    , pre      :: Expression
    , procBody :: Instruction
    , post     :: Expression
    }
  | AbstractProcedureDef
    { pre      :: Expression
    , post     :: Expression
    , params   :: [(Text, Type, ArgMode)]
    }

data Definition
  = Definition
    { defLoc   :: Location
    , defName  :: Text
    , st       :: SymbolTable
    , defBound :: Maybe Expression
    , def'     :: Definition'
    }
  | BadDefinition
    { defLoc :: Location
    }

instance Treelike Definition where
  toTree BadDefinition {defLoc} = leaf $ "Bad Definition " <> show defLoc
  toTree Definition { defLoc, defName, {-st,-} defBound, def' }
    = case def' of
      FunctionDef { funcBody, retType, fparams } ->
        Node ("Function " <> unpack defName <> " -> " <> show retType <> " " <> show defLoc)
          [ Node "Parameters" (showFPs fparams)
          , boundNode
          , Node "Body" [toTree funcBody]
          ]

      ProcedureDef { procDecl, pre, procBody, post, params } ->
        Node ("Procedure " <> unpack defName <> " " <> show defLoc)
          [ Node "Parameters" (showPs params)
          , Node "Declarations" $
              fmap (\x -> case x of; Left a -> toTree a; Right b -> toTree b) procDecl
          , Node "Precondition" [toTree pre]
          , boundNode
          , Node "Body" [toTree procBody]
          , Node "Postcondition" [toTree post]
          ]

      AbstractProcedureDef { pre, post, params} ->
        Node ("Abstarct Procedure " <> unpack defName <> " " <> show defLoc)
          [ Node "Parameters" (showPs params)
          , Node "Precondition" [toTree pre]
          , Node "Postcondition" [toTree post]
          ]

    where
      showPs :: [(Text, Type, ArgMode)] -> [Tree String]
      showPs = fmap (\(n,t,m) -> leaf (show m <> " " <> unpack n <> " : " <> show t))
      showFPs :: [(Text, Type)] -> [Tree String]
      showFPs = fmap (\(n,t) -> leaf (unpack n <> " : " <> show t))
      boundNode = case defBound of
        Just b -> Node "Bound" [toTree b]
        Nothing -> leaf "Not bounded"
