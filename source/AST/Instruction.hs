{-# LANGUAGE NamedFieldPuns #-}

module AST.Instruction where
--------------------------------------------------------------------------------
import           AST.Expression (Expression, Object)
import qualified AST.Expression as E
import           Location
import           SymbolTable
import           Token
import           Treelike
import           Type
--------------------------------------------------------------------------------
import           Data.Monoid    ((<>))
import           Data.Text      (Text, unpack)
--------------------------------------------------------------------------------

{- |
  Tipo de dato que nos permite representar el árbol sintáctico abstracto del
  lenguaje. Los campos @line@ y @column@ representan la línea y columna,
  respectivamente, del nodo en el texto del programa.
 -}

type Guard = (Expression,Instruction)


data Instruction'
  = Abort -- ^ Instruccion Abort.

  | Block
    { blockST    :: SymbolTable
    , blockDecs  :: [Instruction]
    , blockInsts :: [Instruction]
    }

  | Conditional
    { cguards :: [Guard]
    }  -- ^ Instruccion If.

  -- | DecArray
  --   { dimension :: [Expression]
  --   }

  | New
    { idName :: Text
    }

  | Free
    { idName :: Text
    }

  | Assign
    { lvals :: [Object]
    , exprs :: [Expression]
    }
  | Declaration 
    { lvals :: [Object]
    }
  | ProcedureCall
    { pname :: Text
    , astST :: SymbolTable
    , args  :: [Expression]
    }

  | Random
    { var :: Text
    }

  | Read
    { file     :: Maybe Text
    , varTypes :: [Type]
    , vars     :: [(Text, Location)]
    }

  | Repeat
    { rguards :: [Guard]
    , rinv    :: Expression
    , rbound  :: Expression
    } -- ^ Instruccion Do.

  | Skip -- ^ Instruccion Skip.

  | Write
    { ln    :: Bool
    , wexpr :: Expression
    } -- ^ Escribir.


data Instruction
  = Instruction
    { loc   :: Location
    , inst' :: Instruction'
    }
  | NoInstruction
    { loc   :: Location
    }


instance Treelike Instruction where
  toTree Instruction { loc, {-astType,-} inst' } = case inst' of
    Abort ->
      leaf $ "Abort " <> show loc

    Block { blockST, blockDecs, blockInsts } ->
      Node ("Scope " <> show loc)
        [ Node "Declarations" (toForest blockDecs)
        , Node "Actions"      (toForest blockInsts)
        ]

    Conditional { cguards } ->
      Node ("If " <> show loc)
        (map guardToTree cguards)

    New { idName } ->
      Node ("New " <> show loc)
        [leaf . unpack $ idName]

    Free { idName } ->
      Node ("Free " <> show loc)
        [leaf . unpack $ idName]

    Assign { lvals, exprs } ->
      Node "Assignments"
        (zipWith assignToTree lvals exprs)

    ProcedureCall { pname, {-ast,-} args} ->
      Node ("Call Procedure `" <> unpack pname <> "` " <> show loc)
        [Node "Arguments" (toForest args)]

    Random { var } ->
      Node ("Random " <> show loc)
        [leaf . unpack $ var]

    Read file varTypes vars ->
      Node ("Read" <> hasFile <> " " <> show loc)
        (fmap (\(t,(name, l)) ->
                    leaf (unpack name <> " " <> show t <> " " <> show loc))
                 (zip varTypes vars))
      where
        hasFile = case file of
          Nothing -> ""
          Just fileName -> " in file `"<> unpack fileName<>"` "

    Repeat { rguards, rinv, rbound } ->
      Node ("Do " <> show loc) $
        [ Node "Invariant" [toTree rinv]
        , Node "Bound"     [toTree rbound]
        ] <> map guardToTree rguards

    Skip -> leaf $ "Skip " <> show loc

    Write { ln, wexpr } ->
      Node ("Write" <> (if ln then "Ln" else "") <> " " <> show loc)
        [toTree wexpr]

    where
      guardToTree (expr, inst) = Node "Guard"
        [ Node "Condition"   [toTree expr]
        , Node "Instruction" [toTree inst]
        ]
      assignToTree ident expr = Node "(:=)"
        [ toTree ident
        , toTree expr
        ]

  toTree NoInstruction { loc } =
    leaf $ "No instruction " <> show loc
