{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module AST.Expression
  ( BinaryOperator (..)
  , Expression' (..)
  , Expression (..)
  , QRange (..)
  , QuantOperator (..)
  , UnaryOperator (..)
  , Value (..)
  , CollectionKind (..)
  , from
  , to
  , eSkip
  ) where
--------------------------------------------------------------------------------
import {-# SOURCE #-} AST.Type (Type)
--------------------------------------------------------------------------------
import           AST.Object    (Object)
import           Common
--------------------------------------------------------------------------------
import           Data.Array    (Array)
import           Data.List     (intercalate)
import           Data.Sequence (Seq)
import           Data.Text     (Text)
import           Prelude       hiding (Ordering (..))
--------------------------------------------------------------------------------

data BinaryOperator
  = Plus | BMinus | Times | Div | Mod | Power | Max | Min
  | And | Or
  | Implies | Consequent
  | BEQ | BNE
  | AEQ | ANE | LT | LE | GT | GE
  | Elem | NotElem | Difference | Intersection | Union
  | Subset | SSubset | Superset | SSuperset
  | MultisetSum
  | SeqAt
  | BifuncAt
  | Concat
  deriving (Eq)

instance Show BinaryOperator where
  show Plus         = "(+)"
  show BMinus       = "(-)"
  show Times        = "(*)"
  show Div          = "(/)"
  show Mod          = "mod"
  show Power        = "(^)"
  show Max          = "max"
  show Min          = "min"

  show And          = "(/\\)"
  show Or           = "(\\/)"
  show Implies      = "(==>)"
  show Consequent   = "(<==)"
  show BEQ          = "(===)"
  show BNE          = "(!==)"

  show AEQ          = "(==)"
  show ANE          = "(!=)"
  show LT           = "(<)"
  show LE           = "(<=)"
  show GT           = "(>)"
  show GE           = "(>=)"

  show Elem         = "Member of (∈)"
  show NotElem      = "Not Member of (∉)"

  show Difference   = "Difference (∖)"
  show Intersection = "Intersection (∩)"
  show Union        = "Union (∪)"

  show Subset       = "Subset (⊆)"
  show SSubset      = "Strict Subset (⊊)"
  show Superset     = "Superset (⊇)"
  show SSuperset    = "Strict Superset (⊋)"

  show MultisetSum  = "Multiset Sum (⊎)"

  show SeqAt        = "Sequence Access (!)"

  show BifuncAt     = "Function or Relation Access (@)"

  show Concat       = "Sequence Concatenation (++)"



data UnaryOperator = UMinus | Not | Card | Pred | Succ
  deriving (Eq)

instance Show UnaryOperator where
  show UMinus = "(-)"
  show Not    = "not"
  show Card   = "(#)"
  show Pred   = "pred"
  show Succ   = "succ"


data QuantOperator
  = ForAll    | Exists
  | Summation | Product
  | Minimum   | Maximum
  | Count
  deriving (Eq)

instance Show QuantOperator where   -- mempty
  show ForAll    = "Forall (∀)"     -- True
  show Exists    = "Exists (∃)"     -- False
  show Summation = "Summation (∑)"  -- 0, \0, 0.0
  show Product   = "Product (∏)"    -- 1, \1, 1.0
  show Minimum   = "Minimum (min)"  -- ERROR
  show Maximum   = "Maximum (max)"  -- ERROR
  show Count     = "Count (#)"      -- 0


data QRange
  = ExpRange -- Both limits are included, i.e. low <= var <= high
    { low  :: Expression
    , high :: Expression }
  -- | Works for Multiset as well
  | SetRange
    { theSet :: Expression }
  | PointRange
    { thePoint :: Expression }
  | EmptyRange
  deriving (Eq)

instance Show QRange where
  show = \case
    ExpRange { low, high } ->
      unwords [ "from", show low, "to", show high ]
    SetRange { theSet } ->
      unwords [ "in", show theSet ]
    PointRange { thePoint } ->
      unwords [ "at", show thePoint ]
    EmptyRange ->
      "Empty range"


instance Treelike QRange where
  toTree ExpRange { low, high } =
    Node "Exp Range"
      [ Node "From" [toTree low]
      , Node "To"   [toTree high] ]

  toTree SetRange { theSet } =
    Node "Set Range"
      [ Node "Over" [toTree theSet] ]

  toTree PointRange { thePoint } =
    Node "One-point Range"
      [ Node "At" [toTree thePoint] ]

  toTree EmptyRange =
    leaf "Empty Range"


data Value
  = BoolV Bool
  | CharV Char
  | IntV Int32
  | FloatV Double
  deriving (Eq, Ord)

instance Show Value where
  show = \case
    BoolV  v -> show v
    CharV  v -> [v]
    IntV   v -> show v
    FloatV v -> show v

instance Treelike Value where
  toTree = leaf . show


data CollectionKind
  = Set
  | Multiset
  | Sequence
  deriving (Eq, Show)


data Expression'
  = NullPtr
  | Value { theValue :: Value }

  | StringLit { theStringId :: Int }

  | Collection
    { colKind  :: CollectionKind
    , colVar   :: Maybe (Text, Type, QRange, Expression)
      -- ^ the (optional) variable's name, type, range and condition.
    , colElems :: Seq Expression }

  | Tuple
    { left  :: Expression
    , right :: Expression }

  | Obj { theObj :: Object }

  | Binary
    { binOp :: BinaryOperator
    , lexpr :: Expression
    , rexpr :: Expression }

  | Unary
    { unOp  :: UnaryOperator
    , inner :: Expression }

  -- | Cast to an i64 (used for polymorphic functions)
  | I64Cast
    { inner :: Expression }

  -- | Llamada a funcion.
  | FunctionCall
    { fName          :: Text
    , fArgs          :: Seq Expression
    , fRecursiveCall :: Bool
    , fRecursiveFunc :: Bool
    , fStructArgs    :: Maybe (Text, Array Int Type) }

  | Quantification
    { qOp      :: QuantOperator
    , qVar     :: Text
    , qVarType :: Type
    , qRange   :: QRange
    , qCond    :: Expression
    , qBody    :: Expression }

   -- | Expresión If.
  | EConditional
    { eguards    :: Seq (Expression, Expression)
    , trueBranch :: Maybe Expression }

  | RawName
    { theName :: Text }
  deriving (Eq)

data Expression
  = Expression
    { loc      :: Location
    , expType  :: Type
    , expConst :: Bool
    , exp'     :: Expression' }

instance Eq Expression where
  (==)
    (Expression _loc0 expType0 expConst0 exp'0)
    (Expression _loc1 expType1 expConst1 exp'1)
    = expType0 == expType1 && expConst0 == expConst1 && exp'0 == exp'1


eSkip :: Expression'
eSkip = Value . BoolV  $ True


instance Treelike Expression where
  toTree Expression { loc, expType, expConst, exp' } =
    let c = if expConst then "[const] " else "[var] "
    in case exp' of
      NullPtr -> leaf $ "Null Pointer (" <> show expType <> ")"
      Value { theValue } -> leaf $
        case theValue of
          BoolV  v -> "Bool Value `"  <> show v <> "` " <> show loc
          CharV  v -> "Char Value `"  <> show v <> "` " <> show loc
          IntV   v -> "Int Value `"   <> show v <> "` " <> show loc
          FloatV v -> "Float Value `" <> show v <> "` " <> show loc

      StringLit { theStringId } -> leaf $
        "String Literal #" <> show theStringId <> " " <> show loc

      Collection { {-colKind,-} colVar = Nothing, colElems } | null colElems ->
        leaf $ "Empty " <> show expType <> " " <> show loc

      Collection { {-colKind,-} colVar = Nothing, colElems } ->
        Node (show expType <> " " <> show loc)
          [ Node "Elements" (toForest colElems) ]

      Collection { {-colKind,-} colVar = Just (name, ty, range, cond), colElems } ->
        Node (show expType <> " " <> show loc)
          [ Node "Variable"
            [ leaf $ unpack name
            , leaf $ "of type " <> show ty ]
          , Node "Range" [toTree range]
          , case cond of
              Expression { exp' = Value (BoolV True) } -> leaf "No Conditions"
              _ -> Node "Conditions" [ toTree cond]
          , Node "Elements" (toForest colElems) ]

      Tuple { left, right } ->
        Node ("Tuple " <> c <> show loc)
          [ Node "left"  [ toTree left  ]
          , Node "right" [ toTree right ] ]

      Obj { theObj } ->
        Node ("Object " <> show expType <> " " <> c <> show loc)
          [ toTree theObj ]

      Binary { binOp, lexpr, rexpr } ->
        Node (show binOp <> " " <> c <> show loc)
          [ toTree lexpr
          , toTree rexpr ]

      Unary { unOp, inner } ->
        Node (show unOp <> " " <> c <> show loc)
          [ toTree inner ]

      I64Cast { inner } ->
        Node ("I64Cast " <> show loc) [ toTree inner ]

      FunctionCall { fName, fArgs, fRecursiveCall, fRecursiveFunc }
        | fRecursiveCall && fRecursiveFunc ->
          Node ("Recurse " <> c <> show loc)
            [ Node "Arguments" (toForest fArgs) ]
        | otherwise ->
          let rec = if fRecursiveFunc then "Recursive " else ""
          in Node ("Call " <> rec <> "Func " <> unpack fName <> " " <> c <> show loc)
            [ Node "Arguments" (toForest fArgs) ]

      Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } ->
        Node ("Quantification " <> show qOp <> " " <> c <> show loc)
          [ Node "Variable"
            [ leaf $ unpack qVar
            , leaf $ "of type " <> show qVarType ]
          , Node "Range" [toTree qRange]
          , case qCond of
              Expression { exp' = Value (BoolV True) } -> leaf "No Conditions"
              _ -> Node "Conditions" [ toTree qCond ]
          , Node "Body" [ toTree qBody ] ]

      EConditional { eguards, trueBranch } ->
        Node ("Conditional Expression " <> c <> show loc) $
          toList (g <$> eguards) <>
          case trueBranch of
            Just t  -> [ Node "True branch" [toTree t]]
            Nothing -> []

        where
          g (lhs, rhs) =
            Node "Guard"
              [ Node "If"   [toTree lhs]
              , Node "Then" [toTree rhs] ]

      RawName { theName } -> internal $ "A fugitive Raw Name, `" <> unpack theName <> "`"


from :: Expression -> SourcePos
from e = let Location (f,_) = loc e in f

to :: Expression -> SourcePos
to e =   let Location (_,t) = loc e in t


prettyBinOp :: BinaryOperator -> String
prettyBinOp = \case
  Plus         -> " + "
  BMinus       -> " - "
  Times        -> " * "
  Div          -> " / "
  Mod          -> " mod "
  Power        -> " ^ "
  Max          -> " max "
  Min          -> " min "

  And          -> " /\\ "
  Or           -> " \\/ "
  Implies      -> " ==> "
  Consequent   -> " <== "
  BEQ          -> " === "
  BNE          -> " !== "

  AEQ          -> " == "
  ANE          -> " != "
  LT           -> " < "
  LE           -> " <= "
  GT           -> " > "
  GE           -> " >= "

  Elem         -> " ∈ "
  NotElem      -> " ∉ "
  Difference   -> " ∖ "
  Intersection -> " ∩ "
  Union        -> " ∪ "

  Subset       -> " ⊆ "
  SSubset      -> " ⊂ "
  Superset     -> " ⊇ "
  SSuperset    -> " ⊃ "

  MultisetSum  -> " ⊎ "
  SeqAt        -> " [] "
  BifuncAt     -> " () "
  Concat       -> " ⧺ "

prettyUnOp :: UnaryOperator -> String
prettyUnOp = \case
  UMinus -> " - "
  Not    -> " not "
  Card   -> " # "
  Pred   -> "pred"
  Succ   -> "succ"

instance Show Expression where
  show Expression { exp' } = case exp' of
    NullPtr -> "null"
    Value { theValue } -> show theValue

    StringLit { theStringId } -> show theStringId

    Collection { colKind, colVar = Nothing, colElems } | null colElems ->
      show colKind <> "()"

    Collection { colKind, colVar = Nothing, colElems } ->
      show colKind <> "(" <> intercalate ", " (show <$> toList colElems) <> ")"

    Collection { colKind, colVar = Just (name, ty, range, cond), colElems } ->
      show colKind <> unwords
        [ "(", var, ":", ty', "|"
        , range', "|", cond', "|", elems', ")"]
      where
        var    = unpack name
        ty'    = show ty
        range' = show range
        cond'  = show cond
        elems' = intercalate ", " (show <$> toList colElems)

    Obj { theObj } -> show theObj

    Binary { binOp, lexpr, rexpr } ->
      "(" <> show lexpr <> prettyBinOp binOp <> show rexpr <> ")"

    Unary { unOp, inner } ->
      prettyUnOp unOp <> show inner

    FunctionCall { fName, fArgs, fRecursiveCall, fRecursiveFunc }
      | fRecursiveCall && fRecursiveFunc ->
        "(recurse)(" <> (show =<< toList fArgs) <> ")"
      | otherwise ->
        let rec = if fRecursiveFunc then "(rec)" else ""
        in unpack fName <> rec <> "(" <> (show =<< toList fArgs) <> ")"

    Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } ->
      unwords
        [ "(%", op, var, ":", ty, "|"
        , range, "|", cond, "|", body, "%)"]
      where
        op    = case qOp of
          ForAll    -> "∀"
          Exists    -> "∃"
          Summation -> "∑"
          Product   -> "∏"
          Minimum   -> "min"
          Maximum   -> "max"
          Count     -> "#"
        var   = unpack qVar
        ty    = show qVarType
        range = show qRange
        cond  = show qCond
        body  = show qBody

    EConditional { eguards } ->
      "if " <> (showG =<< toList eguards) <> " fi"

      where
        showG (lhs, rhs) =
          show lhs <> " -> " <> show rhs <> "[]"

    _ -> "Show not yet implemented for this expression"
