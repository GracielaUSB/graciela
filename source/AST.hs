{-# LANGUAGE NamedFieldPuns #-}

module AST where
--------------------------------------------------------------------------------
import           SymbolTable
import           Token
import           Treelike
import           Type
import           SourcePos
--------------------------------------------------------------------------------
import           Data.Monoid         ((<>))
import           Data.Text           (Text, unpack)
--------------------------------------------------------------------------------

{- |
  Tipo de dato que nos permite representar el árbol sintáctico abstracto del
  lenguaje. Los campos @line@ y @column@ representan la línea y columna,
  respectivamente, del nodo en el texto del programa.
 -}

data OpNum = Sum | Sub | Mul | Div | Exp | Max | Min | Mod
  deriving (Eq)

instance Show OpNum where
  show Sum = "(+)"
  show Sub = "(-)"
  show Mul = "(*)"
  show Div = "(/)"
  show Exp = "(^)"
  show Max = "max"
  show Min = "min"
  show Mod = "mod"


data OpBool = Dis | Con | Implies | Conse
  deriving (Eq)

instance Show OpBool where
  show Dis     = "(\\/)"
  show Con     = "(/\\)"
  show Implies = "(==>)"
  show Conse   = "(<==)"


data OpRel = Equ | Less | Greater | LEqual | GEqual | Ine
    deriving (Eq)

instance Show OpRel where
  show Equ     = "(==)"
  show Less    = "(<)"
  show Greater = "(>)"
  show LEqual  = "(<=)"
  show GEqual  = "(>=)"
  show Ine     = "(!=)"

data OpSet = Difference | Intersection | Union
instance Show OpSet where
  show Difference   = "Difference (∖)"
  show Intersection = "Intersection (∪)"
  show Union        = "Union (∩)"
  

data Conv = ToInt | ToDouble | ToChar
    deriving (Eq)

instance Show Conv where
  show ToInt    = "to int"
  show ToDouble = "to double"
  show ToChar   = "to char"


data OpUn = Minus | Not | Abs | Sqrt
  deriving (Eq)

instance Show OpUn where
  show Abs    = "abs"
  show Minus  = "(-)"
  show Not    = "not"
  show Sqrt   = "sqrt"


data StateCond
  = Pre | Post | Assertion | Bound | Invariant | Representation | Coupling
  deriving (Eq)

instance Show StateCond where
  show Assertion      = "Aserción"
  show Bound          = "Función de Cota"
  show Coupling       = "Invariante de Acoplamiento"
  show Invariant      = "Invariante"
  show Post           = "Postcondición"
  show Pre            = "Precondición"
  show Representation = "Invariante de Representation"


data QuantOp
  = ForAll    | Exists
  | Summation | Product
  | Minimum   | Maximum
  | Count
  deriving(Eq)

instance Show QuantOp where
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
  toTree _ = leaf "QRange"


data AST'
  = Abort -- ^ Instruccion Abort.
  | Arithmetic -- ^ Operadores Matematicos de dos expresiones.
    { opBinA :: OpNum
    , lexpr  :: AST
    , rexpr  :: AST
    }
  | ArrCall -- ^ Búsqueda en arreglo.
    { arrname :: Text
    , list    :: [AST]
    }
  | Block
    { blockST   :: SymbolTable
    , blockDecs :: [AST]
    , blockActs :: [AST]
    }
  | Bool -- ^ Tipo booleano con el token.
    { cbool :: Bool
    }
  | Boolean
    { opBinB :: OpBool
    , lexpr  :: AST
    , rexpr  :: AST
    }
  | Char
    { mchar    :: Char
    }  -- ^ Tipo caracter con el token.
  | Cond
    { cguard   :: [AST]
    }  -- ^ Instruccion If.
  | Conversion
    { toType :: Conv
    , cExp   :: AST
    }
  | DecArray
    { dimension :: [AST]
    }
  | DefFun
    { dfname    :: Text
    , astST     :: SymbolTable
    , fbody     :: AST
    , retType   :: Type
    , nodeBound :: AST
    , params    :: [(Text, Type)]
    }
  | DefProc
    { pname     :: Text
    , astST     :: SymbolTable
    , prbody    :: AST
    , nodePre   :: AST
    , nodePost  :: AST
    , nodeBound :: AST
    , constDec  :: [AST]
    , params    :: [(Text, Type)]
    }
  | EmptyAST
  | EmptyRange
  | Free 
    { idName :: Text
    }
  | Float
    { expFloat :: Double
    } -- ^ Numero Flotante.
  | FCallExp
    { fname :: Text
    , astST :: SymbolTable         -- ^ Llamada a funcion.
    , args  :: [AST]
    }
  | Id
    { idname :: Text
    } -- ^ Identificador.
  | Int
    { expInt   :: Integer
    } -- ^ Numero entero.
  | Guard
    { gexp     :: AST
    , gact     :: AST               -- ^ Guardia.
    }
  -- | GuardExp
  --   { gexp     :: AST
  --   , gact     :: AST                -- ^ Guardia de Expresion.
  --   }
  | GuardAction
    { assertionGa :: AST
    , actionGa    :: AST
    }
  | LAssign
    { ids   :: [AST]
    , exprs :: [AST]
    }
  | New 
    { idName :: Text
    }
  | ProcCall
    { pname :: Text
    , astST :: SymbolTable
    , args  :: [AST]
    }
  -- | ProcCallCont
  --   { pname :: Text
  --   , astST :: SymbolTable
  --   , args  :: [AST]
  --   , con   :: Contents SymbolTable
  --   }
  | Program
    { pname    :: Text
    , listdef  :: [AST]
    , listacc  :: AST
    }
  | Quantification
    { qOp      :: QuantOp
    , qVar     :: Text
    , qVarType :: Type
    , qRange   :: QRange
    , qCond    :: AST
    , qBody    :: AST
    }
  | Ran
    { var      :: Text
    , retType  :: Type
    }
  | Read
    { file     :: Maybe Text
    , varTypes :: [Type]
    , vars     :: [(Text, SourcePos)]
    }
  | Relational
    { opBinR   :: OpRel -- ^ Operadores Relacionales de dos expresiones.
    , lexpr    :: AST
    , rexp     :: AST
    }
  | Rept
    { rguard   :: [AST]
    , rinv     ::  AST                -- ^ Instruccion Do.
    , rbound   ::  AST
    }
  | Skip -- ^ Instruccion Skip.
  | States
    { tstate   :: StateCond
    , exps     :: AST
    }
  | String
    { mstring  :: String             -- ^ Tipo string con el token.
    } 
  | Unary
    { opUn   :: OpUn
    , lenExp :: AST
    }
  | Write
    { ln   :: Bool
    , wexp :: AST             -- ^ Escribir.
    }


data AST
  = AST
    { posFrom :: SourcePos
    , posTo   :: SourcePos
    , astType :: Type
    , ast'    :: AST'
    }


instance Treelike AST where
  toTree AST { posFrom, posTo, {-astType,-} ast' } = case ast' of
    Abort ->
      leaf $ "Abort " ++ posFrom'

    Arithmetic { opBinA, lexpr, rexpr } ->
      Node (show opBinA ++ posFrom')
        [ toTree lexpr
        , toTree rexpr
        ]

    ArrCall { arrname, list } ->
      Node ("Array Access: `" ++ unpack arrname ++ "`" ++ posFrom')
        (toForest list)

    Block { blockST, blockDecs, blockActs } ->
      Node ("Scope " ++ posFrom')
        [ Node "Declarations" (toForest blockDecs)
        , Node "Actions"      (toForest blockActs)
        ]

    Bool { cbool } ->
      leaf (show cbool ++ posFrom')

    Boolean { opBinB, lexpr, rexpr } ->
      Node (show opBinB ++ posFrom')
        [ toTree lexpr
        , toTree rexpr
        ]

    Char { mchar } ->
      leaf ("`" ++ show mchar ++ "`" ++ posFrom')

    Cond { cguard } ->
      Node ("If " ++ posFrom')
        (toForest cguard)

    Conversion { toType, cExp } ->
      Node (show toType ++ posFrom')
        [toTree cExp]

    DecArray { dimension } ->
      Node "Array Declaration"
        (toForest dimension)

    DefFun { dfname, {-astST,-} fbody, retType, nodeBound, params } ->
      Node ("Function " ++ unpack dfname ++ " -> " ++ show retType ++ posFrom')
        [ Node "Parameters" (showPs params)
        , toTree nodeBound
        , toTree fbody
        ]

    DefProc { pname, {-astST,-} prbody, nodePre, nodePost
            , nodeBound, constDec, params } ->
      Node ("Procedure " ++ unpack pname )
        [ Node "Parameters" (showPs params)
        , Node "Declarations" (toForest constDec)
        , toTree nodePre
        , toTree nodeBound
        , toTree prbody
        , toTree nodePost
        ]

    EmptyAST -> leaf $ "EmptyAST :(" ++ posFrom'

    EmptyRange -> leaf $ "EmptyRange :'(" ++ posFrom'

    Float { expFloat } ->
      leaf ("`" ++ show expFloat ++ "`" ++ posFrom')

    FCallExp { fname, {-astST,-} args } ->
      Node ("Call Func "++unpack fname ++ posFrom')
        [Node "Arguments" (toForest args)]

    Id { idname } ->
      leaf ("`" ++ unpack idname ++ "`" ++ posFrom')

    Int { expInt } ->
      leaf ("`" ++ show expInt ++ "`" ++ posFrom')

    Guard { gexp, gact } ->
      Node "Guard"
        [ Node "Expression" [toTree gexp]
        , Node "Action" [toTree gact]
        ]

    -- GuardExp expr action ->
    --   Node "GuardExp"
    --     [ Node "Expression" [toTree expr]
    --     , Node "Action" [toTree action]
    --     ]

    GuardAction { assertionGa, actionGa } ->
      Node "Guard Action"
        [ Node "Assertion" [toTree assertionGa]
        , Node "Action"    [toTree actionGa]
        ]

    LAssign { ids, exprs } ->
      Node "Assigns"
        (fmap (\(ident,expr) -> Node "(:=)" [toTree ident, toTree expr])
                 (zip ids exprs))

    ProcCall name ast args ->
      Node ("Call Proc "++unpack name ++ posFrom')
        [Node "Arguments" (toForest args)]

    -- ProcCallCont name ast args content ->
    --   Node ("Call Proc Cont (?)"++unpack name ++ posFrom')
    --     [Node "Arguments" (toForest args)]

    Program name defs block ->
      Node ("Program " ++ unpack name ++ posFrom')
        [ Node "Definitions" (toForest defs)
        , toTree block
        ]

    Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } ->
      Node ("Quantification " ++ show qOp ++ posFromTo)
        [ Node "Variable"
          [ leaf $ unpack qVar
          , leaf $ "of type " ++ show qVarType
          ]
        , Node "Range"      [toTree qRange]
        , Node "Conditions" [toTree qCond]
        , Node "Body"       [toTree qBody]
        ]

    Ran var retrn ->
      Node ("Random" ++ posFrom')
        [ leaf ("Variable: "++ unpack var)
        , leaf ("Return: " ++ show retrn)
        ]

    Read file varTypes vars ->
      Node ("Read" ++ hasFile ++ posFrom')
        (fmap (\(t,(name, l)) ->
                    leaf (unpack name ++ " " ++ show t ++ posFrom'))
                 (zip varTypes vars))
      where
        hasFile = case file of
          Nothing -> ""
          Just fileName -> " in file `"++ unpack fileName++"` "

    Relational op l r ->
      Node (show op ++ posFrom') [toTree l, toTree r]

    Rept guard inv bound ->
      Node ("Do" ++ posFrom') (toTree inv : toTree bound : toForest guard)

    Skip -> leaf $ "Skip" ++ posFrom'

    States sc exps ->
      Node (show sc ++ posFrom') [toTree exps]

    String value ->
      leaf ("String: `" ++ value ++ "`" ++ posFrom')

    Unary op expr ->
      Node (show op ++ posFrom') [toTree expr]

    Write ln expr ->
      Node tWrite [toTree expr]
      where tWrite = (if ln then "WriteLn" else "Write") ++ posFrom'

    where
      posFromTo = " (" ++ showPos' posFrom ++ " - " ++ showPos' posTo ++ ")"
      posFrom'  = " "  ++ showPos' posFrom
      showPs    = fmap (\(n,t) -> leaf (unpack n ++ " : " ++ show t))


instance Show AST where
  show = drawTree . toTree
