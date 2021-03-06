{-# LANGUAGE NamedFieldPuns #-}

module Language.Graciela.AST.Instruction where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Declaration (Declaration)
import           Language.Graciela.AST.Expression  (Expression, expType)
import           Language.Graciela.AST.Object      (Object)
import           Language.Graciela.AST.Type        (ArgMode, Type, TypeArgs)
import           Language.Graciela.Common
--------------------------------------------------------------------------------
import           Data.Sequence                     (Seq)
import           Data.Text                         (Text, unpack)
--------------------------------------------------------------------------------

{- |
  Tipo de dato que nos permite representar el árbol sintáctico abstracto del
  lenguaje. Los campos @line@ y @column@ representan la línea y columna,
  respectivamente, del nodo en el texto del programa.
 -}

type Guard = (Expression, Seq Declaration, Seq Instruction)

data Instruction'
  = Abort -- ^ Instruccion Abort.

  | Warn -- ^ Instruccion Warn.

  | Assertion
    { expr :: Expression }

  | Block
    { blockDecs  :: Seq Declaration
    , blockInsts :: Seq Instruction }

  | Conditional
    { cguards :: Seq Guard }  -- ^ Instruccion If.

  | New
    { idName :: Object
    , nType  :: Type }

  | Free
    { idName   :: Object
    , freeType :: Type }

  | Assign
    { assignPairs :: Seq (Object, Expression) }

  | ProcedureCall
    { pName          :: Text
    , pArgs          :: Seq (Expression, ArgMode)
    , pRecursiveCall :: Bool
    , pRecursiveProc :: Bool
    , pStructArgs    :: Maybe (Text, TypeArgs) }

  | Random
    { var :: Object }

  | Read
    { file :: Maybe Text
    , vars :: Seq Object }

  | Repeat
    { rguards :: Seq Guard
    , rinv    :: Expression
    , rbound  :: Expression } -- ^ Instruccion Do.

  | Skip -- ^ Instruccion Skip.

  | Write
    { ln     ::  Bool
    , wexprs :: Seq Expression } -- ^ Escribir.


data Instruction
  = Instruction
    { instLoc :: Location
    , inst'   :: Instruction' }

instance Treelike Instruction where
  toTree Instruction { instLoc, {-astType,-} inst' } = case inst' of
    Abort ->
      leaf $ "Abort " <> show instLoc
    Warn ->
      leaf $ "Warn " <> show instLoc

    Assertion { expr } ->
      Node "Assertion" [toTree expr]

    Block { blockDecs, blockInsts } ->
      Node ("Scope " <> show instLoc)
        [ Node "Declarations" (toForest blockDecs)
        , Node "Actions"      (toForest blockInsts) ]

    Conditional { cguards } ->
      Node ("If " <> show instLoc)
        (toList $ guardToTree <$> cguards)

    New { idName, nType } ->
      Node ("New " <> show instLoc)
        [ toTree idName
        , leaf . show $ nType]

    Free { idName, freeType } ->
      Node ("Free " <> show instLoc)
        [ toTree idName
        , leaf . show $ freeType]

    Assign { assignPairs } ->
      Node "Assignments"
        (toList $ assignToTree <$> assignPairs)

    ProcedureCall { pName, pArgs, pRecursiveCall, pRecursiveProc }
      | pRecursiveCall && pRecursiveProc ->
        Node ("Recurse " <> show instLoc)
          [ if null pArgs
            then leaf "No arguments"
            else Node "Arguments"
              (toList $ (\(x, m) -> Node (show m) [toTree x] ) <$> pArgs) ]

      | otherwise ->
        let
          rec = if pRecursiveProc then "Recursive " else ""
        in Node ("Call " <> rec <> " Procedure `" <> unpack pName <> "` " <> show instLoc)
          [ if null pArgs
            then leaf "No arguments"
            else Node "Arguments"
              (toList $ (\(x, m) -> Node (show m) [toTree x] ) <$> pArgs) ]

    Random { var } ->
      Node ("Random " <> show instLoc)
        [toTree var]

    Read file vars ->
      Node ("Read" <> hasFile <> " " <> show instLoc)
        (toList $ toTree <$> vars)
      where
        hasFile = case file of
          Nothing       -> ""
          Just fileName -> " in file `"<> unpack fileName<>"` "

    Repeat { rguards, rinv, rbound } ->
      Node ("Do " <> show instLoc) $
        [ Node "Invariant" [toTree rinv]
        , Node "Bound"     [toTree rbound]
        ] <> toList (guardToTree <$> rguards)

    Skip -> leaf $ "Skip " <> show instLoc

    Write { ln, wexprs } ->
      Node
        ("Write" <> (if ln then "Ln" else "") <> " " <> show instLoc)
        (fmap writeExp . toList $ wexprs)

    where
      guardToTree (expr, decls, inst) = Node "Guard"
        [ Node "Condition"   [toTree expr]
        , Node "Declarations" $ toForest decls
        , Node "Instructions" $ toForest inst ]
      assignToTree (ident, expr) = Node "(:=)"
        [ toTree ident
        , toTree expr ]
      writeExp e = Node "wexp"
        [ Node "type" [leaf . show . expType $ e]
        , Node "tree" [toTree e] ]
