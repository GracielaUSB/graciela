{-# LANGUAGE NamedFieldPuns #-}

module AST.Expression
  ( Expression (..)
  , Expression' (..)
  , Object (..)
  , BinaryOperator (..)
  , UnaryOperator (..)
  , Conversion (..)
  , QuantOperator (..)
  , from
  , to
  )
  where
--------------------------------------------------------------------------------
import           AST.Object (Object')
import           Location
import           Treelike
import           Type        (Type)
--------------------------------------------------------------------------------
import           Data.Monoid ((<>))
import           Data.Text   (Text, unpack)
import           Prelude     hiding (Ordering (..))
--------------------------------------------------------------------------------

type Object = Object' Expression

data Conversion = ToInt | ToDouble | ToChar
  deriving (Eq)

instance Show Conversion where
  show ToInt    = "to int"
  show ToDouble = "to double"
  show ToChar   = "to char"


data BinaryOperator
  = Plus | BMinus | Times | Div | Mod | Exp | Max | Min
  | And | Or | Implies | Consequent | BEQ | BNE
  | AEQ | ANE | LT | LE | GT | GE
  | Difference | Intersection | Union
  deriving (Eq)

instance Show BinaryOperator where
  show Plus   = "(+)"
  show BMinus = "(-)"
  show Times  = "(*)"
  show Div    = "(/)"
  show Mod    = "mod"
  show Exp    = "(^)"
  show Max    = "max"
  show Min    = "min"

  show And        = "(\\/)"
  show Or         = "(/\\)"
  show Implies    = "(==>)"
  show Consequent = "(<==)"
  show BEQ        = "(===)"
  show BNE        = "(!==)"

  show AEQ = "(==)"
  show ANE = "(!=)"
  show LT  = "(<)"
  show LE  = "(<=)"
  show GT  = "(>)"
  show GE  = "(>=)"

  show Difference   = "Difference (∖)"
  show Intersection = "Intersection (∩)"
  show Union        = "Union (∪)"


data UnaryOperator = UMinus | Not | Abs | Sqrt
  deriving (Eq)

instance Show UnaryOperator where
  show Abs    = "abs"
  show UMinus  = "(-)"
  show Not    = "not"
  show Sqrt   = "sqrt"


data QuantOperator
  = ForAll    | Exists
  | Summation | Product
  | Minimum   | Maximum
  | Count
  deriving(Eq)

instance Show QuantOperator where
  show ForAll    = "Forall (∀)"
  show Exists    = "Exists (∃)"
  show Summation = "Summation (∑)"
  show Product   = "Product (∏)"
  show Minimum   = "Minimum (min)"
  show Maximum   = "Maximum (max)"
  show Count     = "Count (#)"


data QRange
  = QRange
  deriving (Eq)

instance Treelike QRange where
  toTree _ = leaf "QRange" -- Dummy as fuck


data Expression'
  = BoolLit   { theBool   :: Bool    }
  | CharLit   { theChar   :: Char    }
  | FloatLit  { theFloat  :: Double  }
  | IntLit    { theInt    :: Integer }
  | StringLit { theString :: String  }

  | Obj      { theObj :: Object }

  | Binary
    { binOp :: BinaryOperator
    , lexpr :: Expression
    , rexpr :: Expression
    }

  | Unary
    { unOp  :: UnaryOperator
    , inner :: Expression
    }

  | FunctionCall
    { fname :: Text
    -- , astST :: SymbolTable  -- ?
    , args  :: [Expression]
    } -- ^ Llamada a funcion.

  | Conversion
    { toType :: Conversion
    , cExp   :: Expression
    }

  | Quantification
    { qOp      :: QuantOperator
    , qVar     :: Text
    , qVarType :: Type
    , qRange   :: QRange
    , qCond    :: Maybe Expression
    , qBody    :: Expression
    }

data Expression
  = Expression
    { loc     :: Location
    , expType :: Type
    , exp'    :: Expression'
    }
  | BadExpression
    { loc     :: Location
    }


instance Treelike Expression where
  toTree Expression { loc, expType, exp' } = case exp' of
    BoolLit   { theBool   } -> leaf $
      "Bool Literal `"   <> show theBool   <> "` " <> show loc

    CharLit   { theChar   } -> leaf $
      "Char Literal "    <> show theChar   <> " "  <> show loc

    FloatLit  { theFloat  } -> leaf $
      "Float Literal `"  <> show theFloat  <> "` " <> show loc

    IntLit    { theInt    } -> leaf $
      "Int Literal `"    <> show theInt    <> "` " <> show loc

    StringLit { theString } -> leaf $
      "String Literal `" <> show theString <> "` " <> show loc

    Obj { theObj } ->
      Node ("Object " <> show loc)
        [toTree theObj]

    Binary { binOp, lexpr, rexpr } ->
      Node (show binOp <> " " <> show loc)
        [ toTree lexpr
        , toTree rexpr
        ]

    Unary { unOp, inner } ->
      Node (show unOp <> " " <> show loc)
        [ toTree inner
        ]

    FunctionCall { fname, {-astST,-} args } ->
      Node ("Call Func " <> unpack fname <> " " <> show loc)
        [ Node "Arguments" (toForest args)
        ]

    Conversion { toType, cExp } ->
      Node (show toType <> " " <> show loc)
        [toTree cExp]

    Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } ->
      Node ("Quantification " <> show qOp <> " " <> show loc)
        [ Node "Variable"
          [ leaf $ unpack qVar
          , leaf $ "of type " <> show qVarType
          ]
        , Node "Range"      [toTree qRange]
        , case qCond of
          Just cond -> Node "Conditions" [toTree cond]
          Nothing   -> leaf "No Conditions"
        , Node "Body"       [toTree qBody]
        ]

  toTree BadExpression { loc } =
    leaf $ "Bad Expression " <> show loc


from :: Expression -> SourcePos
from Expression    { loc = Location (f,_)} = f
from BadExpression { loc = Location (f,_)} = f

to :: Expression -> SourcePos
to Expression    { loc = Location (_,t)} = t
to BadExpression { loc = Location (_,t)} = t
