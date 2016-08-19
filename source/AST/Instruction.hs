{-# LANGUAGE NamedFieldPuns #-}

module AST.Instruction where
--------------------------------------------------------------------------------
import           AST.Declaration (Declaration)
import           AST.Expression  (Expression, Object)
import qualified AST.Expression  as E
import           Type        (Type)
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


type Guard = (Expression,[Declaration],[Instruction])

data Instruction'
  = Abort -- ^ Instruccion Abort.
  | Assertion
    { expr :: Expression
    }
  | Block
    { blockST    ::  SymbolTable
    , blockDecs  :: [Declaration]
    , blockInsts :: [Instruction]
    }

  | Conditional
    { cguards :: [Guard]
    }  -- ^ Instruccion If.

  -- | DecArray
  --   { dimension :: [Expression]
  --   }

  | New
    { idName :: Object
    , nType  :: Type
    }

  | Free
    { idName    :: Object
    , freeType  :: Type
    }

  | Assign
    { lvals :: [Object]
    , exprs :: [Expression]
    }
  | ProcedureCall
    { pname :: Text
    {-, astST :: SymbolTable-}
    , args  :: [Expression]
    }

  | Random
    { var :: Object
    }

  | Read
    { file     :: Maybe Text
    , varTypes :: [Type]
    , vars     :: [Object]
    }

  | Repeat
    { rguards :: [Guard]
    , rinv    :: Expression
    , rbound  :: Expression
    } -- ^ Instruccion Do.

  | Skip -- ^ Instruccion Skip.

  | Write
    { ln     ::  Bool
    , wexprs :: [Expression]
    } -- ^ Escribir.


data Instruction
  = Instruction
    { instLoc :: Location
    , inst'   :: Instruction'
    }
  | BadInstruction
    { instLoc :: Location
    }


instance Treelike Instruction where
  toTree Instruction { instLoc, {-astType,-} inst' } = case inst' of
    Abort ->
      leaf $ "Abort " <> show instLoc

    Assertion { expr } ->
      Node "Assertion" [toTree expr]

    Block { blockST, blockDecs, blockInsts } ->
      Node ("Scope " <> show instLoc)
        [ Node "Declarations" (toForest blockDecs)
        , Node "Actions"      (toForest blockInsts)
        ]

    Conditional { cguards } ->
      Node ("If " <> show instLoc)
        (fmap guardToTree cguards)

    New { idName, nType } ->
      Node ("New " <> show instLoc)
        [toTree idName
        ,leaf . show $ nType]

    Free { idName, freeType } ->
      Node ("Free " <> show instLoc)
        [toTree idName
        ,leaf . show $ freeType]

    Assign { lvals, exprs } ->
      Node "Assignments"
        (zipWith assignToTree lvals exprs)

    ProcedureCall { pname, {-ast,-} args} ->
      Node ("Call Procedure `" <> unpack pname <> "` " <> show instLoc)
        [case args of
          [] -> leaf "No arguments"
          _  -> Node "Arguments" (toForest args)
        ]

    Random { var } ->
      Node ("Random " <> show instLoc)
        [toTree var]

    Read file varTypes vars ->
      Node ("Read" <> hasFile <> " " <> show instLoc)
        (fmap toTree vars)
      where
        hasFile = case file of
          Nothing -> ""
          Just fileName -> " in file `"<> unpack fileName<>"` "

    Repeat { rguards, rinv, rbound } ->
      Node ("Do " <> show instLoc) $
        [ Node "Invariant" [toTree rinv]
        , Node "Bound"     [toTree rbound]
        ] <> fmap guardToTree rguards

    Skip -> leaf $ "Skip " <> show instLoc

    Write { ln, wexprs } ->
      Node ("Write" <> (if ln then "Ln" else "") <> " " <> show instLoc) $
        toForest wexprs

    where
      guardToTree (expr, decls, inst) = Node "Guard"
        [ Node "Condition"   [toTree expr]
        , Node "Declarations" $ toForest decls
        , Node "Instructions" $ toForest inst
        ]
      assignToTree ident expr = Node "(:=)"
        [ toTree ident
        , toTree expr
        ]


  toTree BadInstruction { instLoc } =
    leaf $ "No instruction " <> show instLoc
