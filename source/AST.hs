{-# LANGUAGE NamedFieldPuns #-}

module AST where
--------------------------------------------------------------------------------
import           SymbolTable
import           Token
import           Treelike
import           Type
--------------------------------------------------------------------------------
import           Contents            (Contents)
import           Data.Monoid         ((<>))
-- import           Data.Range.Range    (Range)
import           Data.Text           (Text, unpack)
import           Text.Megaparsec.Pos (SourcePos (..))
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
  deriving (Eq)
  
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


data QuantOp = ForAll | Exists | Summation | Product | Minimum | Maximum
  deriving(Eq)

instance Show QuantOp where
  show ForAll    = "Forall (∀)"
  show Exists    = "Exists (∃)"
  show Summation = "Summatory (∑)"
  show Product   = "Productory (∏)"
  show Minimum   = "Minimum (min)"
  show Maximum   = "Maximum (max)"


-- --Rangos
-- data OpSet = Union | Intersec
--   deriving (Eq)
--
-- data UnknownRange
--   = SetRange
--     { getOp   :: OpSet
--     , getLexp :: UnknownRange
--     , getRexp :: UnknownRange
--     }
--   | TupleRange
--     { getLeft  :: AST
--     , getRight :: AST
--     }
--   deriving (Eq)


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
  -- | ConsAssign
  --   { caIds   :: [(Text, SourcePos)]
  --   , caExprs :: [AST]
  --   }
  -- | Constant
  --   { int       :: Bool                  -- ^ Constantes.
  --   , max       :: Bool
  --   }
  | Conversion
    { cToType :: Conv
    , cExp   :: AST
    }
  | DecArray
    { dimension :: [AST]
    }
  | DefFunc
    { dfname    :: Text
    , astST     :: SymbolTable
    , fbody     :: AST
    , retType   :: Type
    -- , nodeBound :: AST
    , params    :: [(Text, Type)]
    }
  | DefProc
    { pname     :: Text
    , astST     :: SymbolTable
    , prbody    :: AST
    , nodePre   :: AST
    , nodePost  :: AST
    -- , nodeBound :: AST
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
  | ProcCallCont
    { pname :: Text
    , astST :: SymbolTable
    , args  :: [AST]
    , con   :: Contents SymbolTable
    }
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
  deriving (Eq)


data AST
  = AST
    { posFrom :: SourcePos
    , posTo   :: SourcePos
    , astType :: Type
    , ast'    :: AST'
    }
  deriving (Eq)


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

    -- ConsAssign { caIds, caExprs } ->
    --   let ids = fmap (\(t,l) -> leaf (unpack t ++ " " ++ showPos' l)) caIds in
    --   Node ("ConsAssign" ++ posFromTo)
    --     [ Node "IDs" ids
    --     , Node "Expresions" (toForest caExprs)
    --     ]

    -- Constant int isMax ->
    --   leaf (checkMaxMin int isMax ++ posFrom')

    Conversion { cToType, cExp } ->
      Node (show cToType ++ posFrom')
        [toTree cExp]

    DecArray { dimension } ->
      Node "Array Declaration"
        (toForest dimension)

    DefFunc { dfname, {-astST,-} fbody, retType, {-nodeBound,-} params } ->
      Node ("Function " ++ unpack dfname ++ " -> " ++ show retType ++ posFrom')
        [ Node "Parameters" (showPs params)
        , toTree fbody
        ]

    DefProc { pname, {-astST,-} prbody, nodePre, nodePost
            , {-nodeBound,-} constDec, params } ->
      Node ("Procedure " ++ unpack pname )
        [ Node "Parameters" (showPs params)
        , Node "Declarations" (toForest constDec)
        , toTree nodePre
        -- , toTree nodeBound
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

    ProcCallCont name ast args content ->
      Node ("Call Proc Cont (?)"++unpack name ++ posFrom')
        [Node "Arguments" (toForest args)]

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

astToId :: AST -> Maybe Text
astToId AST { ast' = (Id i)        } = Just i
astToId AST { ast' = (ArrCall i _) } = Just i
astToId AST { ast' = _             } = Nothing


instance Show AST where
  show = drawTree . toTree


checkMaxMin :: Bool -> Bool -> String
checkMaxMin True  True  = "MAX_INT"
checkMaxMin True  False = "MIN_INT"
checkMaxMin False True  = "MAX_DOUBLE"
checkMaxMin False False = "MIN_DOUBLE"


checkWrite :: Bool -> String
checkWrite True  = "Escribir con Salto de línea"
checkWrite False = "Escribir"


putSourcePos :: SourcePos -> String
putSourcePos position = " --- en el " <> show position


putSourcePosLn :: SourcePos -> String
putSourcePosLn position = " --- en el " <> show position <> "\n"


-- drawAST :: Int -> AST -> String
-- drawAST level (Program name defs accs) =
--   putSpacesLn level <> "Programa: " <> show name <> putSourcePos pos
--                      <> " //Tag: "   <> show ast
--                      <> drawASTList (level + 4) defs <> drawAST (level + 4) accs
--
--
-- drawAST level (DefProc name _ accs pre post bound _ _ ast) =
--   putSpacesLn level <> "Procedimiento: " <> show name  -- <> putSourcePos pos
--                      <> " //Tag: "        <> show ast
--                      <> putSpaces (level + 4)   <> drawAST (level + 4) pre
--                      <> putSpaces (level + 4)   <> drawAST (level + 8) bound
--                      <> putSpacesLn (level + 4) <> "Acciones: " <> drawAST (level + 8) accs
--                      <> putSpaces (level + 4)   <> drawAST (level + 4) post
--
--
-- drawAST level (States t pos expr ast) =
--   putSpacesLn level <> show t     <> putSourcePos pos
--                      <> " //Tag: " <> show ast
--                      <> drawAST (level + 4) expr
--
--
-- drawAST level (Arithmetic t pos lexpr rexp ast) =
--   putSpacesLn   level <> "Operador Aritmético: " <> show t <> putSourcePos pos
--                        <> " //Tag: "              <> show ast
--                        <> putSpacesLn (level + 4) <> "Lado izquierdo:"
--                                                          <> drawAST (level + 8) lexpr
--                        <> putSpacesLn (level + 4) <> "Lado derecho:"
--                                                          <> drawAST (level + 8) rexp
--
--
-- drawAST level (Relational t pos lexpr rexp ast) =
--   putSpacesLn level <> "Operador Relacional: " <> show t <> putSourcePos pos
--                      <> " //Tag: "              <> show ast
--                      <> putSpacesLn (level + 4) <> "Lado izquierdo:"
--                                                        <> drawAST (level + 8) lexpr
--                      <> putSpacesLn (level + 4) <> "Lado derecho:"
--                                                        <> drawAST (level + 8) rexp
--
--
-- drawAST level (Boolean t pos lexpr rexp ast) =
--   putSpacesLn level <> "Operador Booleano: "   <> show t <> putSourcePos pos
--                      <> " //Tag: "              <> show ast
--                      <> putSpacesLn (level + 4) <> "Lado izquierdo:"
--                                                        <> drawAST (level + 8) lexpr
--                      <> putSpacesLn (level + 4) <> "Lado derecho:"
--                                                        <> drawAST (level + 8) rexp
--
--
-- drawAST level (Id pos cont ast) =
--   putSpacesLn level <> "Id: "        <> show cont <> putSourcePos pos
--                      <> " //Tag: "    <> show ast
--
--
-- drawAST level (Int pos cont ast) =
--   putSpacesLn level <> "Entero: "    <> show cont <> putSourcePos pos
--                      <> " //Tag: "    <> show ast
--
--
-- drawAST level (Float pos cont ast) =
--   putSpacesLn level <> "Flotante: "  <> show cont <> putSourcePos pos
--                      <> " //Tag: "    <> show ast
--
--
-- drawAST level (Bool pos cont ast) =
--   putSpacesLn level <> "Booleano: "  <> show cont <> putSourcePos pos
--                      <> " //Tag: "    <> show ast
--
--
-- drawAST level (Char pos cont ast) =
--   putSpacesLn level <> "Caracter: "  <> show cont <> putSourcePos pos
--                      <> " //Tag: "    <> show ast
--
--
-- drawAST level (String pos cont ast) =
--   putSpacesLn level <> "String: "    <> show cont <> putSourcePos pos
--                      <> " //Tag: "    <> show ast
--
--
-- -- drawAST level (Constant pos t max ast) =
-- --   putSpacesLn level <> "Constante: " <> checkMaxMin t max <> putSourcePos pos
-- --                      <> " //Tag: "    <> show ast
--
--
-- drawAST level (LAssign idlist explist pos ast) =
--   putSpacesLn level <> "Asignación: " <> putSourcePos pos
--                      <> " //Tag: "     <> show ast
--                      <> drawLAssign level idlist explist
--
--
-- drawAST level (Rept guard inv bound pos ast) =
--   putSpacesLn level <> "Repetición: " <> putSourcePos pos
--                      <> " //Tag: "     <> show ast
--                      <> drawASTList (level + 4) guard
--                      <> putSpaces   (level + 4) <> drawAST (level + 4) inv
--                      <> putSpaces   (level + 4) <> drawAST (level + 4) bound
--
--
-- drawAST level (Cond guard pos ast) =
--   putSpacesLn level <> "Condicional: " <> putSourcePos pos
--                      <> " //Tag: "      <> show ast
--                      <> drawASTList (level + 4) guard
--
--
-- drawAST level (Guard exp action pos ast) =
--   putSpacesLn level <> "Guardia: " <> putSourcePos pos
--                      <> " //Tag: "  <> show ast
--                      <> drawAST (level + 4) exp
--                      <> putSpacesLn (level + 4) <> "Acciones:"
--                      <> drawAST (level + 8) action
--
--
-- -- drawAST level (GuardExp exp action pos ast) =
-- --   putSpacesLn level <> "Guardia de Expresión: " <> putSourcePos pos
-- --                      <> " //Tag: "               <> show ast
-- --                      <> drawAST (level + 4) exp
-- --                      <> putSpacesLn (level + 4) <> "Expresión:"
-- --                      <> drawAST (level + 8) action
--
--
-- drawAST level (Block pos st _ action ast) =
--   putSpacesLn level <> "Bloque: " <> putSourcePos pos
--                      <> " //Tag: " <> show ast  <> show st
--                      <> drawASTList (level + 4) action
--
--
-- drawAST level (Skip pos ast) =
--   putSpacesLn level <> "Saltar: " <> putSourcePos pos
--                      <> " //Tag: " <> show ast
--
--
-- drawAST level (Abort pos ast) =
--   putSpacesLn level <> "Abortar: " <> putSourcePos pos
--                      <> " //Tag: "  <> show ast
--
--
-- drawAST level (Ran var _ pos ast) =
--   putSpacesLn level <> "Aleatorio: " <> show var <> putSourcePos pos
--                      <> " //Tag: "    <> show ast
--
--
-- drawAST level (Write ln exp pos ast) =
--   putSpacesLn level <> checkWrite ln <> putSourcePos pos
--                      <> " //Tag: "  <> show ast
--                      <> drawAST (level + 4) exp
--
--
-- drawAST level (GuardAction pos assert action  ast) =
--   putSpacesLn level <> "Guardia de Acción: " <> putSourcePos pos
--                      <> " //Tag: "            <> show ast
--                      <> drawAST (level + 4) assert
--                      <> putSpacesLn (level + 4) <> "Acciones:"
--                      <> drawAST (level + 8) action
--
--
--
-- -- drawAST level (Quant op var pos range term ast) =
-- --   putSpacesLn level <> "Cuantificador: " <> show op <> putSourcePos pos
-- --                      <> " //Tag: "        <> show ast
-- --   <> putSpacesLn (level + 4) <> "Variable cuantificada: " <> show var
-- --   <> putSpacesLn (level + 4) <> "Rango: "  <> drawAST (level + 8) range
-- --   <> putSpacesLn (level + 4) <> "Cuerpo: " <> drawAST (level + 8) term
--
--
--
-- drawAST level (Conversion t pos exp ast) =
--   putSpacesLn level <> "Conversión: " <> show t <> putSourcePos pos
--                      <> " //Tag: "     <> show ast
--                      <> drawAST (level + 4) exp
--
--
--
-- drawAST level (Unary op pos exp ast) =
--   putSpacesLn level <> show op    <> putSourcePos pos
--                      <> " //Tag: " <> show ast
--                      <> drawAST (level + 4) exp
--
--
--
-- drawAST level (FCallExp name _ pos args ast) =
--   putSpacesLn level <> "Llamada de la Función: " <> show name <> putSourcePos pos
--                      <> " //Tag: "                <> show ast
--   <> putSpacesLn (level + 4) <> "Argumentos: " <> drawASTList (level + 8) args
--
--
--
-- drawAST level (ArrCall pos name args ast) =
--   putSpacesLn level <> "Llamada del Arreglo: " <> show name <> putSourcePos pos
--                      <> " //Tag: "              <> show ast
--   <> putSpacesLn (level + 4) <> "Argumentos: " <> drawASTList (level + 8) args
--
--
--
-- drawAST level (ProcCall name st pos args ast) =
--   putSpacesLn level <> "Llamada del Procedimiento: " <> show name <> putSourcePos pos
--                      <> " //Tag: "                    <> show ast
--   <> putSpacesLn (level + 4) <> "Argumentos: " <> drawASTList (level + 8) args
--   <> "------------------------------------------------------------------------------------"
--   <> show st
--
--
-- drawAST level (DefFunc name st _ body _ bound _ ast) =
--   putSpacesLn level <> "Función: " <> show name
--                      <> " //Tag: "   <> show ast
--                      <> drawAST(level + 4) bound
--                      <> drawAST(level + 4) body
--
--
-- drawAST _ (EmptyRange _ _) = "rango vacio"
-- drawAST _ (EmptyAST _) = "vacio"
--
-- drawAST _ _ = "undefined"
--
-- drawASTList level = foldl (\acc d -> (acc <> drawAST level d)) []
--
--
-- drawLAssign level idlist explist = foldl (\acc (id, exp) ->
--   (acc <> putSpacesLn (level + 4) <> "Variable: " <> show id
--         <> putSpacesLn (level + 8) <> "Lado derecho: "
--         <> drawAST (level + 12) exp)) [] $ zip idlist explist
