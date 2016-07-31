{-# LANGUAGE NamedFieldPuns #-}

module AST.Definition where
--------------------------------------------------------------------------------
import           AST.Expression  (Expression)
import qualified AST.Expression  as E
import           AST.Instruction (Instruction)
import qualified AST.Instruction as I
import           Location
import           SymbolTable
import           Treelike
import           Type            (Type)
import qualified Type            as T
--------------------------------------------------------------------------------
import           Data.Monoid     ((<>))
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

data Definition'
  = FunctionDef
    { funcbody :: Expression
    , retType  :: Type
    }
  | ProcedureDef
    { constDec :: [Instruction] -- ?
    , pre      :: Expression
    , procbody :: Instruction
    , post     :: Expression
    }

data Definition
  = Definition
    { loc    :: Location
    , name   :: Text
    , params :: [(Text, Type)]
    , st     :: SymbolTable
    , bound  :: Maybe Expression
    , def'   :: Definition'
    }

instance Treelike Definition where
  toTree Definition { from, to, name, params, {-st,-} bound, def' }
    = case def' of

      FunctionDef { funcbody, retType } ->
        Node ("Function " <> unpack name <> " -> " <> show retType <> " " <> show loc)
          [ Node "Parameters" (showPs params)
          , boundNode
          , Node "Body" [toTree funcbody]
          ]

      ProcedureDef { constDec, pre, procbody, post } ->
        Node ("Procedure " <> unpack name <> " " <> show loc)
          [ Node "Parameters" (showPs params)
          , Node "Declarations" (toForest constDec)
          , Node "Precondition" [toTree pre]
          , boundNode
          , Node "Body" [toTree procbody]
          , Node "Postcondition" [toTree post]
          ]

    where
      showPs = fmap (\(n,t) -> leaf (unpack n ++ " : " ++ show t))
      boundNode = case bound of
        Just b -> Node "Bound" [toTree b]
        Nothing -> leaf "Not bounded"
