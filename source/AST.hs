module AST where
--------------------------------------------------------------------------------
import           SymbolTable
import           Token
import           Treelike
import           Type
--------------------------------------------------------------------------------
import           Contents            (Contents)
import           Data.Monoid         ((<>))
import           Data.Range.Range    (Range)
import           Data.Text           (Text, unpack)
import           Text.Megaparsec.Pos (SourcePos (..))
--------------------------------------------------------------------------------

{- |
   Tipo de dato que nos permite representar el árbol sintáctico abstracto
   del lenguaje. Los campos @line@ y @column@ representan la línea y columna, respectivamente,
   del nodo en el texto del programa.
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


data Conv = ToInt | ToDouble | ToChar
      deriving (Eq)

instance Show Conv where
   show ToInt    = "to int"
   show ToDouble = "to double"
   show ToChar   = "to char"


data OpUn = Minus | Not | Abs | Sqrt
      deriving (Eq)

instance Show OpUn where
   show Minus  = "(-)"
   show Not    = "not"
   show Abs    = "abs"
   show Sqrt   = "sqrt"


data StateCond = Pre | Post | Assertion | Bound | Invariant | Representation | Couple
      deriving (Eq)

instance Show StateCond where
   show Pre       = "Precondición"
   show Post      = "Postcondición"
   show Assertion = "Aserción"
   show Bound     = "Función de Cota"
   show Invariant = "Invariante"
   show Representation = "Invariante de Representation"
   show Couple    = "Invariante de Acoplamiento"


data OpQuant = ForAll | Exists | Summation | Product | Minimum | Maximum
    deriving(Eq)

instance Show OpQuant where
   show ForAll    = "Forall (∀)"
   show Exists    = "Exists (∃)"
   show Summation = "Summatory (∑)"
   show Product   = "Productory (∏)"
   show Minimum   = "Minimum (min)"
   show Maximum   = "Maximum (max)"


--Rangos
data OpSet = Union | Intersec
      deriving (Eq)

data UnknownRange = SetRange   { getOp :: OpSet, getLexp :: UnknownRange, getRexp :: UnknownRange }
                  | TupleRange { getLeft :: AST Type, getRight :: AST Type }
      deriving (Eq)


data AST a = Abort      { position  :: SourcePos, tag         :: a } -- ^ Instruccion Abort.
         | Arithmetic   { opBinA    :: OpNum   , position    :: SourcePos -- ^ Operadores Matematicos de dos expresiones.
                        , lexpr     :: AST a   , rexp        :: AST a
                        , tag :: a
                        }
         | ArrCall      { position  :: SourcePos, name        :: Text      -- ^ Búsqueda en arreglo.
                        , list      :: [AST a] , tag         :: a
                        }
         | Bposk        { position  :: SourcePos, bposkStable :: SymbolTable
                        , listDec   :: [AST a] , lisAct      :: [AST a]
                        , tag       :: a
                        }
         | Bool         { position  :: SourcePos, cbool       :: Bool   , tag :: a } -- ^ Tipo booleano con el token.

         | Boolean      { opBinB    :: OpBool  , position    :: SourcePos -- ^ Operadores Booleanos de dos expresiones.
                        , lexpr     :: AST a   , rexp        :: AST a
                        , tag :: a
                        }
         | Char         { position  :: SourcePos, mchar       :: Char    , tag :: a }  -- ^ Tipo caracter con el token.

         | Cond         { cguard    :: [AST a] , position    :: SourcePos, tag :: a }  -- ^ Instruccion If.

         | ConsAssign   { position  :: SourcePos, caId        :: [(Text, SourcePos)]
                        , caExpr    :: [AST a] , tag         :: a
                        }
         | Constant     { position  :: SourcePos, int         :: Bool                  -- ^ Constantes.
                        , max       ::  Bool   , tag         :: a
                        }
         | Conversion   { toType    :: Conv    , position    :: SourcePos              -- ^ Conversión a entero.
                        , tiexp     :: AST a   , tag         :: a
                        }
         | DecArray     { dimension :: [AST a] , tag         :: a }

         | DefFun       { dfname    ::  Text   , astSTable   :: SymbolTable
                        , position  :: SourcePos, fbody       :: AST a
                        , retType   ::  Type   , nodeBound   :: AST a
                        , params    ::[(Text, Type)], tag    :: a
                        }
         | DefProc      { pname     ::  Text   , astSTable   :: SymbolTable, prbody    :: AST a
                        , nodePre   ::  AST a  , nodePost    ::  AST a     , nodeBound :: AST a
                        , constDec  :: [AST a] , params      :: [(Text, Type)]
                        , tag       ::  a
                        }
         | EmptyAST     { tag ::  a }

         | EmptyRange   { position  :: SourcePos, tag         :: a }

         | Float        { position  :: SourcePos, expFloat    :: Double , tag :: a } -- ^ Numero Flotante.

         | FCallExp     { fname     ::  Text   , astSTable   :: SymbolTable         -- ^ Llamada a funcion.
                        , position  :: SourcePos, args        :: [AST a], tag :: a
                        }
         | Id           { position  :: SourcePos, id          :: Text   , tag :: a } -- ^ Identificador.

         | Int          { position  :: SourcePos, expInt      :: Integer, tag :: a } -- ^ Numero entero.

         | Guard        { gexp      ::  AST a  , gact        :: AST a               -- ^ Guardia.
                        , position  :: SourcePos, tag         :: a
                        }
         | GuardExp     { gexp      ::  AST a  , gact        :: AST a                -- ^ Guardia de Expresion.
                        , position  :: SourcePos, tag         :: a
                        }
         | GuardAction  { position  :: SourcePos, assertionGa :: AST a
                        , actionGa  :: AST a   , tag         :: a
                        }
         | LAssign      { idlist    :: [AST a] , explista    :: [AST a]
                        , position  :: SourcePos, tag         :: a
                        }
         | ProcCall     { pname     ::  Text   , astSTable   :: SymbolTable
                        , position  :: SourcePos, args        :: [AST a]
                        , tag       :: a
                        }
         | ProcCallCont { pname     ::  Text   , astSTable   :: SymbolTable
                        , position  :: SourcePos, args        :: [AST a]
                        , con       :: Contents SymbolTable, tag :: a
                        }

         | Program      { pname     :: Text    , position    :: SourcePos
                        , listdef   :: [AST a] , listacc     :: AST a, tag :: a
                        }
         | Quant        { opQ       :: OpQuant , varQ        :: Text
                        , position  :: SourcePos, rangeExp    :: AST a
                        , termExpr  :: AST a   , tag         :: a
                        }
         | QuantRan     { opQ       :: OpQuant , varQ        :: Text
                        , position  :: SourcePos, rangeVExp   :: [Range Integer]
                        , termExpr  :: AST a   , tag         :: a
                        }
         | QuantRanUn   { opQ       :: OpQuant , varQ        :: Text
                        , position  :: SourcePos, rangeUExp   :: UnknownRange
                        , termExpr  :: AST a   , tag         :: a
                        }
         | Ran          { var       :: Text    , retType     :: Type
                        , position  :: SourcePos, tag         :: a
                        }
         | Read         { position  :: SourcePos, file        :: Maybe String
                        , varTypes  :: [Type]  , vars        :: [(Text, SourcePos)], tag :: a
                        }
         | Relational   { opBinR    :: OpRel   , position    :: SourcePos -- ^ Operadores Relacionales de dos expresiones.
                        , lexpr     :: AST a   , rexp        :: AST a
                        , tag       :: a
                        }
         | Rept         { rguard    :: [AST a] , rinv        :: AST a                -- ^ Instruccion Do.
                        , rbound    ::  AST a  , position    :: SourcePos , tag :: a
                        }
         | Skip         { position  :: SourcePos, tag         :: a } -- ^ Instruccion Skip.

         | States       { tstate    :: StateCond, position   :: SourcePos
                        , exps      :: AST a    , tag        :: a
                        }
         | String       { position  :: SourcePos, mstring     :: String , tag :: a } -- ^ Tipo string con el token.

         | Unary        { opUn      :: OpUn    , position    :: SourcePos             -- ^ Función raíz cuadrada.
                        , lenExp    :: AST a   , tag         :: a
                        }
         | Write        { ln        ::  Bool   , wexp        :: AST a             -- ^ Escribir.
                        , position  :: SourcePos, tag         :: a
                        }

    deriving (Eq)

instance Show a => Treelike (AST a) where
   toTree (Abort l _) =
      leaf $ "Abort " ++ showPos' l

   toTree (Arithmetic op pos l r _)  =
      Node (show op ++ "  " ++showPos' pos) [toTree l, toTree r]

   toTree (ArrCall pos n list _) =
      Node ("Array Access: `" ++ unpack n ++ "` " ++ showPos' pos) (toForest list)

   toTree (Bposk pos st decls actions _ ) =
      Node ("Scope " ++ showPos' pos)
         [Node "Declarations" (toForest decls),
          Node "Actions"     (toForest actions)]

   toTree (Bool pos value _) =
      leaf (show value ++ " " ++ showPos' pos)

   toTree (Boolean op pos l r _) =
      Node (show op ++ "  " ++showPos' pos) [toTree l, toTree r]

   toTree (Char pos value _) =
      leaf ("`" ++ show value ++ "` " ++ showPos' pos)

   toTree (Cond guard pos _) =
      Node ("If " ++ showPos' pos) (toForest guard)

   toTree (ConsAssign pos ids exprs _) =
         Node ("ConsAssign " ++ showPos' pos)
            [ Node "IDs" ( fmap (\(t,l) -> leaf (unpack t ++ " " ++ showPos' l)) ids)
            , Node "Expresions" (toForest exprs) ]

   toTree (Constant pos int isMax _) =
      leaf (checkMaxMin int isMax ++ " " ++ showPos' pos)

   toTree (Conversion to pos expr _) =
      Node (show to ++ " " ++ showPos' pos) [toTree expr]

   toTree (DecArray dim _) =
      Node ("Array Declaration") (toForest dim)

   toTree (DefFun name st pos body retrn bound params _) =
      Node ("Function " ++ unpack name ++ " -> " ++ show retrn ++ " " ++ showPos' pos)
         [ Node "Parameters" (fmap (\(pname,t) ->
               leaf (unpack pname ++ " : " ++ show t)) params)
         , toTree bound -- WTF?
         , toTree body
         ]

   toTree (DefProc name ast body pre post bound decl params _ ) =
      Node ("Procedure " ++ unpack name )
         [ Node "Parameters" (fmap (\(pname,t) ->
               leaf (unpack pname ++ " : " ++ show t)) params)
         , Node "Declarations" (toForest decl)
         , toTree pre
         , toTree bound -- WTF?
         , toTree body
         , toTree post
         ]

   toTree (EmptyAST _ ) = leaf "EmptyAST :("

   toTree (EmptyRange pos _ ) = leaf ("EmptyRange :'( " ++ showPos' pos)

   toTree (Float pos value _) =
      leaf ("`" ++ show value ++ "` " ++ showPos' pos)

   toTree (FCallExp name ast pos args _) =
      Node ("Call Func "++unpack name ++ " " ++ showPos' pos)
         [Node "Arguments" (toForest args)]

   toTree (Id pos name _) =
      leaf ("`" ++ unpack name ++ "` " ++ showPos' pos)

   toTree (Int pos value _) =
      leaf ("`" ++ show value ++ "` " ++ showPos' pos)

   toTree (Guard expr action pos _) =
      Node "Guard"
         [ Node "Expression" [toTree expr]
         , Node "Action" [toTree action]
         ]

   toTree (GuardExp expr action pos _) =
      Node "GuardExp"
         [ Node "Expression" [toTree expr]
         , Node "Action" [toTree action]
         ]

   toTree (GuardAction pos assert action _) =
      Node "Guard Action"
         [ Node "Assertion" [toTree assert]
         , Node "Action"    [toTree action]
         ]

   toTree (LAssign ids exprs pos _) =
      Node "Assigns"
         (fmap (\(ident,expr) -> Node "(:=)" [toTree ident, toTree expr])
               (zip ids exprs))

   toTree (ProcCall name ast pos args _) =
      Node ("Call Proc "++unpack name ++ " " ++ showPos' pos)
         [Node "Arguments" (toForest args)]

   toTree (ProcCallCont name ast pos args content _) =
      Node ("Call Proc Cont (?)"++unpack name ++ " " ++ showPos' pos)
         [Node "Arguments" (toForest args)]

   toTree (Program name pos defs bposk _) =
      Node ("Program " ++ unpack name ++ " " ++ showPos' pos)
         [ Node "Definitions" (toForest defs)
         , toTree bposk
         ]

   toTree (Quant op var pos range body _) =
      Node ("Quantifier " ++ show op ++ " " ++ showPos' pos)
         [ leaf ("Variable: " ++ unpack var)
         , Node "Range" [toTree range]
         , Node "Body"  [toTree body]
         ]

   toTree (QuantRan op var pos range body _) =
      Node ("Quantifier " ++ show op ++ " " ++ showPos' pos)
         [ leaf ("Variable: " ++ unpack var)
         , Node "Range" (fmap (leaf . show) range)
         , Node "Body"  [toTree body]
         ]

   toTree (QuantRanUn op var pos range body _) =
      Node ("Quantifier " ++ show op ++ " " ++ showPos' pos)
         [ leaf ("Variable: " ++ unpack var)
         , leaf "Unknown Range"
         , Node "Body"  [toTree body]
         ]

   toTree (Ran var retrn pos _) =
      Node ("Random " ++ showPos' pos)
         [ leaf ("Variable: "++ unpack var)
         , leaf ("Return: " ++ show retrn)
         ]

   toTree (Read pos file varTypes vars _) =
      Node ("Read" ++ hasFile ++ showPos' pos)
         (fmap (\(t,(name, l)) ->
                  leaf (unpack name ++ " " ++ show t ++ " " ++ showPos' pos))
               (zip varTypes vars))
      where
         hasFile = case file of
            Nothing -> ""
            Just fileName -> " in file `"++fileName++"` "

   toTree (Relational op pos l r _)  =
      Node (show op ++ "  " ++showPos' pos) [toTree l, toTree r]

   toTree (Rept guard inv bound pos _) =
      Node ("Do " ++ showPos' pos) (toTree inv : toTree bound : toForest guard)

   toTree (Skip pos _) = leaf $ "Skip "++ showPos' pos

   toTree (States sc pos exps _) =
      Node (show sc ++ " " ++ showPos' pos) [toTree exps]

   toTree (String pos value _) =
      leaf ("String: `" ++ value ++ "` " ++ showPos' pos)

   toTree (Unary op pos expr _) =
      Node (show op ++ " " ++ showPos' pos) [toTree expr]

   toTree (Write ln expr pos _) =
      Node tWrite [toTree expr]
      where tWrite = (if ln then "WriteLn " else "Write ") ++ showPos' pos

  --  toTree e = Node "You're my Creator but you never teach me how to draw myself as a Tree :''("
  --              [ leaf (show e)]



astToId :: AST a -> Maybe Text
astToId (Id _ id _) = Just id
astToId (ArrCall _ id _ _) = Just id
astToId _           = Nothing

instance Show a => Show (AST a) where
    show = drawAST 0


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


drawAST :: Show a => Int -> AST a -> String
drawAST level (Program name pos defs accs ast) =
   putSpacesLn level <> "Programa: " <> show name <> putSourcePos pos
                     <> " //Tag: "   <> show ast
                     <> drawASTList (level + 4) defs <> drawAST (level + 4) accs


drawAST level (DefProc name _ accs pre post bound _ _ ast) =
   putSpacesLn level <> "Procedimiento: " <> show name  -- <> putSourcePos pos
                     <> " //Tag: "        <> show ast
                     <> putSpaces (level + 4)   <> drawAST (level + 4) pre
                     <> putSpaces (level + 4)   <> drawAST (level + 8) bound
                     <> putSpacesLn (level + 4) <> "Acciones: " <> drawAST (level + 8) accs
                     <> putSpaces (level + 4)   <> drawAST (level + 4) post


drawAST level (States t pos expr ast) =
   putSpacesLn level <> show t     <> putSourcePos pos
                     <> " //Tag: " <> show ast
                     <> drawAST (level + 4) expr


drawAST level (Arithmetic t pos lexpr rexp ast) =
   putSpacesLn   level <> "Operador Aritmético: " <> show t <> putSourcePos pos
                       <> " //Tag: "              <> show ast
                       <> putSpacesLn (level + 4) <> "Lado izquierdo:"
                                                         <> drawAST (level + 8) lexpr
                       <> putSpacesLn (level + 4) <> "Lado derecho:"
                                                         <> drawAST (level + 8) rexp


drawAST level (Relational t pos lexpr rexp ast) =
   putSpacesLn level <> "Operador Relacional: " <> show t <> putSourcePos pos
                     <> " //Tag: "              <> show ast
                     <> putSpacesLn (level + 4) <> "Lado izquierdo:"
                                                       <> drawAST (level + 8) lexpr
                     <> putSpacesLn (level + 4) <> "Lado derecho:"
                                                       <> drawAST (level + 8) rexp


drawAST level (Boolean t pos lexpr rexp ast) =
   putSpacesLn level <> "Operador Booleano: "   <> show t <> putSourcePos pos
                     <> " //Tag: "              <> show ast
                     <> putSpacesLn (level + 4) <> "Lado izquierdo:"
                                                       <> drawAST (level + 8) lexpr
                     <> putSpacesLn (level + 4) <> "Lado derecho:"
                                                       <> drawAST (level + 8) rexp


drawAST level (Id pos cont ast) =
   putSpacesLn level <> "Id: "        <> show cont <> putSourcePos pos
                     <> " //Tag: "    <> show ast


drawAST level (Int pos cont ast) =
   putSpacesLn level <> "Entero: "    <> show cont <> putSourcePos pos
                     <> " //Tag: "    <> show ast


drawAST level (Float pos cont ast) =
   putSpacesLn level <> "Flotante: "  <> show cont <> putSourcePos pos
                     <> " //Tag: "    <> show ast


drawAST level (Bool pos cont ast) =
   putSpacesLn level <> "Booleano: "  <> show cont <> putSourcePos pos
                     <> " //Tag: "    <> show ast


drawAST level (Char pos cont ast) =
   putSpacesLn level <> "Caracter: "  <> show cont <> putSourcePos pos
                     <> " //Tag: "    <> show ast


drawAST level (String pos cont ast) =
   putSpacesLn level <> "String: "    <> show cont <> putSourcePos pos
                     <> " //Tag: "    <> show ast


drawAST level (Constant pos t max ast) =
   putSpacesLn level <> "Constante: " <> checkMaxMin t max <> putSourcePos pos
                     <> " //Tag: "    <> show ast


drawAST level (LAssign idlist explist pos ast) =
   putSpacesLn level <> "Asignación: " <> putSourcePos pos
                     <> " //Tag: "     <> show ast
                     <> drawLAssign (level) idlist explist


drawAST level ((Rept guard inv bound pos ast)) =
   putSpacesLn level <> "Repetición: " <> putSourcePos pos
                     <> " //Tag: "     <> show ast
                     <> drawASTList (level + 4) guard
                     <> putSpaces   (level + 4) <> drawAST (level + 4) inv
                     <> putSpaces   (level + 4) <> drawAST (level + 4) bound


drawAST level ((Cond guard pos ast)) =
   putSpacesLn level <> "Condicional: " <> putSourcePos pos
                     <> " //Tag: "      <> show ast
                     <> drawASTList (level + 4) guard


drawAST level ((Guard exp action pos ast)) =
   putSpacesLn level <> "Guardia: " <> putSourcePos pos
                     <> " //Tag: "  <> show ast
                     <> drawAST (level + 4) exp
                     <> putSpacesLn (level + 4) <> "Acciones:"
                     <> drawAST (level + 8) action


drawAST level ((GuardExp exp action pos ast)) =
   putSpacesLn level <> "Guardia de Expresión: " <> putSourcePos pos
                     <> " //Tag: "               <> show ast
                     <> drawAST (level + 4) exp
                     <> putSpacesLn (level + 4) <> "Expresión:"
                     <> drawAST (level + 8) action


drawAST level ((Bposk pos st _ action ast)) =
   putSpacesLn level <> "Bloque: " <> putSourcePos pos
                     <> " //Tag: " <> show ast  <> show st
                     <> drawASTList (level + 4) action


drawAST level ((Skip pos ast)) =
   putSpacesLn level <> "Saltar: " <> putSourcePos pos
                     <> " //Tag: " <> show ast


drawAST level ((Abort pos ast)) =
   putSpacesLn level <> "Abortar: " <> putSourcePos pos
                     <> " //Tag: "  <> show ast


drawAST level ((Ran var _ pos ast)) =
   putSpacesLn level <> "Aleatorio: " <> show var <> putSourcePos pos
                     <> " //Tag: "    <> show ast


drawAST level ((Write ln exp pos ast)) =
   putSpacesLn level <> checkWrite ln <> putSourcePos pos
                     <> " //Tag: "  <> show ast
                     <> drawAST (level + 4) (exp)


drawAST level ((GuardAction pos assert action  ast)) =
   putSpacesLn level <> "Guardia de Acción: " <> putSourcePos pos
                     <> " //Tag: "            <> show ast
                     <> drawAST (level + 4) assert
                     <> putSpacesLn (level + 4) <> "Acciones:"
                     <> drawAST (level + 8) action



drawAST level ((Quant op var pos range term ast)) =
   putSpacesLn level <> "Cuantificador: " <> show op <> putSourcePos pos
                     <> " //Tag: "        <> show ast
   <> putSpacesLn (level + 4) <> "Variable cuantificada: " <> show var
   <> putSpacesLn (level + 4) <> "Rango: "  <> drawAST (level + 8) range
   <> putSpacesLn (level + 4) <> "Cuerpo: " <> drawAST (level + 8) term



drawAST level ((Conversion t pos exp ast)) =
   putSpacesLn level <> "Conversión: " <> show t <> putSourcePos pos
                     <> " //Tag: "     <> show ast
                     <> drawAST (level + 4) exp



drawAST level ((Unary op pos exp ast)) =
   putSpacesLn level <> show op    <> putSourcePos pos
                     <> " //Tag: " <> show ast
                     <> drawAST (level + 4) exp



drawAST level ((FCallExp name _ pos args ast)) =
   putSpacesLn level <> "Llamada de la Función: " <> show name <> putSourcePos pos
                     <> " //Tag: "                <> show ast
   <> putSpacesLn (level + 4) <> "Argumentos: " <> drawASTList (level + 8) args



drawAST level ((ArrCall pos name args ast)) =
   putSpacesLn level <> "Llamada del Arreglo: " <> show name <> putSourcePos pos
                     <> " //Tag: "              <> show ast
   <> putSpacesLn (level + 4) <> "Argumentos: " <> drawASTList (level + 8) args



drawAST level ((ProcCall name st pos args ast)) =
   putSpacesLn level <> "Llamada del Procedimiento: " <> show name <> putSourcePos pos
                     <> " //Tag: "                    <> show ast
   <> putSpacesLn (level + 4) <> "Argumentos: " <> drawASTList (level + 8) args
   <> "------------------------------------------------------------------------------------"
   <> show st


drawAST level ((DefFun name st _ body _ bound _ ast)) =
   putSpacesLn level <> "Función: " <> show name
                     <> " //Tag: "   <> show ast
                     <> drawAST(level + 4) bound
                     <> drawAST(level + 4) body


drawAST _ (EmptyRange _ _) = "rango vacio"
drawAST _ (EmptyAST _) = "vacio"

drawAST _ _ = "undefined"

drawASTList level xs = foldl (\acc d -> (acc <> drawAST level (d))) [] xs


drawLAssign level idlist explist = foldl (\acc (id, exp) ->
   (acc <> putSpacesLn (level + 4) <> "Variable: " <> show (id)
        <> putSpacesLn (level + 8) <> "Lado derecho: "
        <> drawAST (level + 12) exp)) [] $ zip idlist explist
