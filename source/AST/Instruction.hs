{-# LANGUAGE NamedFieldPuns #-}

module AST.Instruction where
--------------------------------------------------------------------------------
import           AST.Declaration (Declaration)
import           AST.Expression  (Expression, Object)
import qualified AST.Expression  as E
import           Location
import           SymbolTable
import           Token
import           Treelike
import           Type
--------------------------------------------------------------------------------
import           Data.Foldable   (toList)
import           Data.Monoid     ((<>))
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq (zipWith)
import           Data.Text       (Text, unpack)
--------------------------------------------------------------------------------

{- |
  Tipo de dato que nos permite representar el árbol sintáctico abstracto del
  lenguaje. Los campos @line@ y @column@ representan la línea y columna,
  respectivamente, del nodo en el texto del programa.
 -}

type Guard = (Expression, {- whyyy? [Declaration], -} Seq Instruction)

data Instruction'
  = Abort -- ^ Instruccion Abort.
  | Assertion
    { expr :: Expression
    }
  | Block
    { blockDecs  :: Seq Declaration
    , blockInsts :: Seq Instruction
    }

  | Conditional
    { cguards :: Seq Guard
    }  -- ^ Instruccion If.

  | New
    { idName :: Object
    , nType  :: Type
    }

  | Free
    { idName   :: Object
    , freeType :: Type
    }

  | Assign
    { assignPairs :: Seq (Object, Expression)
    --
    -- lvals :: Seq Object
    -- , exprs :: Seq Expression
    }
  | ProcedureCall
    { pname :: Text
    {-, astST :: SymbolTable-}
    , pargs :: Seq (Expression, ArgMode)
    }

  | Random
    { var :: Object
    }

  | Read
    { file :: Maybe Text
    , vars :: Seq Object
    }

  | Repeat
    { rguards :: Seq Guard
    , rinv    :: Expression
    , rbound  :: Expression
    } -- ^ Instruccion Do.

  | Skip -- ^ Instruccion Skip.

  | Write
    { ln     ::  Bool
    , wexprs :: Seq Expression
    } -- ^ Escribir.


data Instruction
  = Instruction
    { instLoc :: Location
    , inst'   :: Instruction'
    }

instance Treelike Instruction where
  toTree Instruction { instLoc, {-astType,-} inst' } = case inst' of
    Abort ->
      leaf $ "Abort " <> show instLoc

    Assertion { expr } ->
      Node "Assertion" [toTree expr]

    Block { blockDecs, blockInsts } ->
      Node ("Scope " <> show instLoc)
        [ Node "Declarations" (toForest blockDecs)
        , Node "Actions"      (toForest blockInsts)
        ]

    Conditional { cguards } ->
      Node ("If " <> show instLoc)
        (toList $ guardToTree <$> cguards)

    New { idName, nType } ->
      Node ("New " <> show instLoc)
        [toTree idName
        ,leaf . show $ nType]

    Free { idName, freeType } ->
      Node ("Free " <> show instLoc)
        [toTree idName
        ,leaf . show $ freeType]

    Assign { assignPairs } ->
      Node "Assignments"
        (toList $ assignToTree <$> assignPairs)

    ProcedureCall { pname, {-ast,-} pargs} ->
      Node ("Call Procedure `" <> unpack pname <> "` " <> show instLoc)
        [ if null pargs
          then leaf "No arguments"
          else Node "Arguments"
            (toList $ (\(x, m) -> Node (show m) [toTree x] ) <$> pargs)
        ]

    Random { var } ->
      Node ("Random " <> show instLoc)
        [toTree var]

    Read file vars ->
      Node ("Read" <> hasFile <> " " <> show instLoc)
        (toList $ toTree <$> vars)
      where
        hasFile = case file of
          Nothing -> ""
          Just fileName -> " in file `"<> unpack fileName<>"` "

    Repeat { rguards, rinv, rbound } ->
      Node ("Do " <> show instLoc) $
        [ Node "Invariant" [toTree rinv]
        , Node "Bound"     [toTree rbound]
        ] <> toList (guardToTree <$> rguards)

    Skip -> leaf $ "Skip " <> show instLoc

    Write { ln, wexprs } ->
      Node ("Write" <> (if ln then "Ln" else "") <> " " <> show instLoc) $
        toForest wexprs

    where
      guardToTree (expr, {-decls,-} inst) = Node "Guard"
        [ Node "Condition"   [toTree expr]
        -- , Node "Declarations" $ toForest decls
        , Node "Instructions" $ toForest inst
        ]
      assignToTree (ident, expr) = Node "(:=)"
        [ toTree ident
        , toTree expr
        ]
