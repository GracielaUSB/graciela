{-# LANGUAGE NamedFieldPuns #-}

module AST.Instruction where
--------------------------------------------------------------------------------
import           AST.Expression (Expression)
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

data Instruction'
  = Abort -- ^ Instruccion Abort.

  | Block
    { blockST    :: SymbolTable
    , blockDecs  :: [Instruction]
    , blockInsts :: [Instruction]
    }

  | Conditional
    { cguards :: [(Expression, Instruction)]
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
    { ids   :: [Text]
    , exprs :: [Expression]
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
    , vars     :: [(Text, SourcePos)]
    }

  | Repeat
    { rguards :: [(Expression, Instruction)]
    , rinv    :: Instruction
    , rbound  :: Instruction
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

    Assign { ids, exprs } ->
      Node "Assignments"
        (zipWith assignToTree ids exprs)

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
        [ leaf $ unpack ident
        , toTree expr
        ]

  toTree NoInstruction { from, to } =
    leaf $ "No instruction " <> show loc
