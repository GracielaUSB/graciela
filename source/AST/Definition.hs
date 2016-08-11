{-# LANGUAGE NamedFieldPuns #-}

module AST.Definition where
--------------------------------------------------------------------------------
import           AST.Declaration  (Declaration)
import           AST.Expression  (Expression)
import qualified AST.Expression  as E
import           AST.Instruction (Instruction)
import qualified AST.Instruction as I
import           AST.Type        (Type)
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
    }
  | ProcedureDef
    { procDecl :: [Either Declaration Instruction] 
    , pre      :: Expression
    , procBody :: Instruction
    , post     :: Expression
    }
  | AbstractProcedureDef
    { pre  :: Expression
    , post :: Expression
    }

data Definition
  = Definition
    { defLoc   :: Location
    , defName  :: Text
    , params   :: [(Text, Type)]
    , st       :: SymbolTable
    , defBound :: Maybe Expression
    , def'     :: Definition'
    }
  | BadDefinition
    { defLoc :: Location
    }

instance Treelike Definition where
  toTree BadDefinition {defLoc} = leaf $ "Bad Definition " <> show defLoc
  toTree Definition { defLoc, defName, params, {-st,-} defBound, def' }
    = case def' of

      FunctionDef { funcBody, retType } ->
        Node ("Function " <> unpack defName <> " -> " <> show retType <> " " <> show defLoc)
          [ Node "Parameters" (showPs params)
          , boundNode
          , Node "Body" [toTree funcBody]
          ]

      ProcedureDef { procDecl, pre, procBody, post } ->
        Node ("Procedure " <> unpack defName <> " " <> show defLoc)
          [ Node "Parameters" (showPs params)
          , Node "Declarations" $ 
              fmap (\x -> case x of; Left a -> toTree a; Right b -> toTree b) procDecl
          , Node "Precondition" [toTree pre]
          , boundNode
          , Node "Body" [toTree procBody]
          , Node "Postcondition" [toTree post]
          ]

      ProcedureDef { pre, post } ->
        Node ("Procedure " <> unpack defName <> " " <> show defLoc)
          [ Node "Parameters" (showPs params)
          , Node "Precondition" [toTree pre]
          , Node "Postcondition" [toTree post]
          ]
  
    where
      showPs = fmap (\(n,t) -> leaf (unpack n <> " : " <> show t))
      boundNode = case defBound of
        Just b -> Node "Bound" [toTree b]
        Nothing -> leaf "Not bounded"
