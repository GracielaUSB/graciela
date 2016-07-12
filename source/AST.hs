module AST where
--------------------------------------------------------------------------------
-- import           Limits
import           Location
import           SymbolTable
import           Type
import           Treelike
--------------------------------------------------------------------------------
import           Contents         (Contents)
import           Data.Monoid      ((<>))
import           Data.Range.Range (Range)
import           Data.Text        (Text, unpack)
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


data AST a = Abort      { location  :: Location, tag         :: a } -- ^ Instruccion Abort.
         | Arithmetic   { opBinA    :: OpNum   , location    :: Location -- ^ Operadores Matematicos de dos expresiones.
                        , lexpr     :: AST a   , rexp        :: AST a
                        , tag :: a
                        }
         | ArrCall      { location  :: Location, name        :: Text      -- ^ Búsqueda en arreglo.
                        , list      :: [AST a] , tag         :: a
                        } 
         | Block        { location  :: Location, blockStable :: SymbolTable
                        , listDec   :: [AST a] , lisAct      :: [AST a]
                        , tag       :: a
                        }
         | Bool         { location  :: Location, cbool       :: Bool   , tag :: a } -- ^ Tipo booleano con el token.
         | Boolean      { opBinB    :: OpBool  , location    :: Location -- ^ Operadores Booleanos de dos expresiones.
                        , lexpr     :: AST a   , rexp        :: AST a
                        , tag :: a
                        }
         | Char         { location  :: Location, mchar       :: Char    , tag :: a }  -- ^ Tipo caracter con el token.
         | Cond         { cguard    :: [AST a] , location    :: Location, tag :: a }  -- ^ Instruccion If.
         | ConsAssign   { location  :: Location, caId        :: [(Text, Location)]
                        , caExpr    :: [AST a] , tag         :: a             
                        }
         | Constant     { location  :: Location, int         :: Bool                  -- ^ Constantes.
                        , max       ::  Bool   , tag         :: a
                        }  
         | Conversion   { toType    :: Conv    , location    :: Location              -- ^ Conversión a entero.
                        , tiexp     :: AST a   , tag         :: a
                        }
         | DecArray     { dimension :: [AST a] , tag         :: a }
         | DefFun       { dfname    ::  Text   , astSTable   :: SymbolTable
                        , location  :: Location, fbody       :: AST a
                        , retType   ::  Type   , nodeBound   :: AST a
                        , params    ::[(Text, Type)], tag    :: a 
                        }
         | DefProc      { pname     ::  Text   , astSTable   :: SymbolTable, prbody    :: AST a
                        , nodePre   ::  AST a  , nodePost    ::  AST a     , nodeBound :: AST a
                        , constDec  :: [AST a] , params      :: [(Text, Type)]    
                        , tag       ::  a
                        }
         | EmptyAST     { tag       ::  a                                         }
         | EmptyRange   { location  :: Location, tag         :: a                 }
         | Float        { location  :: Location, expFloat    :: Double , tag :: a } -- ^ Numero entero.
         | FCallExp     { fname     ::  Text   , astSTable   :: SymbolTable       -- ^ Llamada a funcion.
                        , location  :: Location, args        :: [AST a], tag :: a
                        } 
         | Id           { location  :: Location, id          :: Text   , tag :: a } -- ^ Identificador.
         | Int          { location  :: Location, expInt      :: Integer, tag :: a } -- ^ Numero entero.     
         | Guard        { gexp      ::  AST a  , gact        :: AST a      -- ^ Guardia.
                        , location  :: Location, tag         :: a 
                        }
         | GuardExp     { gexp      ::  AST a  , gact        :: AST a      -- ^ Guardia de Expresion.
                        , location  :: Location, tag         :: a 
                        }
         | GuardAction  { location  :: Location, assertionGa :: AST a
                        , actionGa  :: AST a   , tag         :: a
                        }
         | LAssign      { idlist    :: [AST a] , explista    :: [AST a] 
                        , location  :: Location, tag         :: a
                        } 
         | ProcCall     { pname     ::  Text   , astSTable   :: SymbolTable
                        , location  :: Location, args        :: [AST a] 
                        , tag       :: a
                        }
         | ProcCallCont { pname     ::  Text   , astSTable   :: SymbolTable
                        , location  :: Location, args        :: [AST a]
                        , con       :: Contents SymbolTable, tag :: a 
                        }

         | Program      { pname     :: Text    , location    :: Location
                        , listdef   :: [AST a] , listacc     :: AST a, tag :: a 
                        }
         | Quant        { opQ       :: OpQuant , varQ        :: Text
                        , location  :: Location, rangeExp    :: AST a
                        , termExpr  :: AST a   , tag         :: a
                        }
         | QuantRan     { opQ       :: OpQuant , varQ        :: Text
                        , location  :: Location, rangeVExp   :: [Range Integer]
                        , termExpr  :: AST a   , tag         :: a
                        }
         | QuantRanUn   { opQ       :: OpQuant , varQ        :: Text
                        , location  :: Location, rangeUExp   :: UnknownRange
                        , termExpr  :: AST a   , tag         :: a
                        }
         | Ran          { var       :: Text    , retType     :: Type
                        , location  :: Location, tag         :: a
                        }
         | Read         { location  :: Location, file        :: Maybe String
                        , varTypes  :: [Type]  , vars        :: [(Text, Location)], tag :: a
                        }
         | Relational   { opBinR    :: OpRel   , location    :: Location -- ^ Operadores Relacionales de dos expresiones.
                        , lexpr     :: AST a   , rexp        :: AST a
                        , tag       :: a
                        } 
         | Rept         { rguard    :: [AST a] , rinv        :: AST a                -- ^ Instruccion Do.
                        , rbound    ::  AST a  , location    :: Location , tag :: a  
                        } 
         | Skip         { location  :: Location, tag         :: a } -- ^ Instruccion Skip.
         | States       { tstate    :: StateCond, location   :: Location
                        , exps      :: AST a    , tag        :: a
                        }         
         | String       { location  :: Location, mstring     :: String , tag :: a } -- ^ Tipo string con el token.
         | Unary        { opUn      :: OpUn    , location    :: Location             -- ^ Función raíz cuadrada.
                        , lenExp    :: AST a   , tag         :: a
                        }
         | Write        { ln        ::  Bool   , wexp        :: AST a             -- ^ Escribir.
                        , location  :: Location, tag         :: a
                        } 

    deriving (Eq)

instance Show a => Treelike (AST a) where
   toTree (Abort l _) = 
      leaf $ "Abort " ++ showL l

   toTree (Arithmetic op loc l r _)  =
      Node (show op ++ "  " ++showL loc) [toTree l, toTree r]

   toTree (ArrCall loc n list _) = 
      Node ("Array Access: `" ++ unpack n ++ "` " ++ showL loc) (toForest list)

   toTree (Block loc st decls actions _ ) = 
      Node ("Scope " ++ showL loc) 
         [Node "Declarations" (toForest decls),
          Node "Actions"     (toForest actions)]

   toTree (Bool loc value _) = 
      leaf (show value ++ " " ++ showL loc)

   toTree (Boolean op loc l r _) = 
      Node (show op ++ "  " ++showL loc) [toTree l, toTree r]

   toTree (Char loc value _) = 
      leaf ("`" ++ show value ++ "` " ++ showL loc)

   toTree (Cond guard loc _) = 
      Node ("If " ++ showL loc) (toForest guard)

   toTree (ConsAssign loc ids exprs _) = 
         Node ("ConsAssign " ++ showL loc) 
            [ Node "IDs" ( fmap (\(t,l) -> leaf (unpack t ++ " " ++ showL l)) ids)
            , Node "Expresions" (toForest exprs) ]
 
   toTree (Constant loc int isMax _) = 
      leaf (checkMaxMin int isMax ++ " " ++ showL loc)

   toTree (Conversion to loc expr _) = 
      Node (show to ++ " " ++ showL loc) [toTree expr]

   toTree (DecArray dim _) = 
      Node ("Array Declaration") (toForest dim)

   toTree (DefFun name st loc body retrn bound params _) = 
      Node ("Function " ++ unpack name ++ " -> " ++ show retrn ++ " " ++ showL loc) 
         [ Node "Parameters" (fmap (\(pname,t) -> 
               leaf (unpack pname ++ " : " ++ show t)) params)
         , toTree bound
         , toTree body 
         ]

   toTree (DefProc name ast body pre post bound decl params _ ) =
      Node ("Procedure " ++ unpack name ) 
         [ Node "Parameters" (fmap (\(pname,t) -> 
               leaf (unpack pname ++ " : " ++ show t)) params)
         , Node "Declarations" (toForest decl)
         , toTree pre 
         , toTree bound
         , toTree body
         , toTree post 
         ]

   toTree (EmptyAST _ ) = leaf "EmptyAST :("

   toTree (EmptyRange loc _ ) = leaf ("EmptyRange :'( " ++ showL loc)   

   toTree (Float loc value _) = 
      leaf ("`" ++ show value ++ "` " ++ showL loc)

   toTree (FCallExp name ast loc args _) =
      Node ("Call Func "++unpack name ++ " " ++ showL loc) 
         [Node "Arguments" (toForest args)]

   toTree (Id loc name _) = 
      leaf ("`" ++ unpack name ++ "` " ++ showL loc)
 
   toTree (Int loc value _) = 
      leaf ("`" ++ show value ++ "` " ++ showL loc)

   toTree (Guard expr action loc _) = 
      Node "Guard" 
         [ Node "Expression" [toTree expr]
         , Node "Action" [toTree action]
         ]

   toTree (GuardExp expr action loc _) = 
      Node "GuardExp" 
         [ Node "Expression" [toTree expr]
         , Node "Action" [toTree action]
         ]

   toTree (GuardAction loc assert action _) = 
      Node "Guard Action" 
         [ Node "Assertion" [toTree assert]
         , Node "Action"    [toTree action]
         ]

   toTree (LAssign ids exprs loc _) = 
      Node "Assigns" 
         (fmap (\(ident,expr) -> Node "(:=)" [toTree ident, toTree expr])
               (zip ids exprs))

   toTree (ProcCall name ast loc args _) =
      Node ("Call Proc "++unpack name ++ " " ++ showL loc) 
         [Node "Arguments" (toForest args)]

   toTree (ProcCallCont name ast loc args content _) =
      Node ("Call Proc Cont (?)"++unpack name ++ " " ++ showL loc) 
         [Node "Arguments" (toForest args)]

   toTree (Program name loc defs block _) = 
      Node ("Program " ++ unpack name ++ " " ++ showL loc)
         [ Node "Definitions" (toForest defs)
         , toTree block
         ]

   toTree (Quant op var loc range body _) = 
      Node ("Quantifier " ++ show op ++ " " ++ showL loc)
         [ leaf ("Variable: " ++ unpack var)
         , Node "Range" [toTree range]
         , Node "Body"  [toTree body]
         ]

   toTree (QuantRan op var loc range body _) = 
      Node ("Quantifier " ++ show op ++ " " ++ showL loc)
         [ leaf ("Variable: " ++ unpack var)
         , Node "Range" (fmap (leaf . show) range)
         , Node "Body"  [toTree body]
         ]

   toTree (QuantRanUn op var loc range body _) = 
      Node ("Quantifier " ++ show op ++ " " ++ showL loc)
         [ leaf ("Variable: " ++ unpack var)
         , leaf "Unknown Range"
         , Node "Body"  [toTree body]
         ]

   toTree (Ran var retrn loc _) = 
      Node ("Random " ++ showL loc)
         [ leaf ("Variable: "++ unpack var)
         , leaf ("Return: " ++ show retrn)
         ]

   toTree (Read loc file varTypes vars _) = 
      Node ("Read" ++ hasFile ++ showL loc)
         (fmap (\(t,(name, l)) -> 
                  leaf (unpack name ++ " " ++ show t ++ " " ++ showL loc))
               (zip varTypes vars))
      where 
         hasFile = case file of 
            Nothing -> ""
            Just fileName -> " in file `"++fileName++"` "

   toTree (Relational op loc l r _)  =
      Node (show op ++ "  " ++showL loc) [toTree l, toTree r]

   toTree (Rept guard inv bound loc _) = 
      Node ("Do " ++ showL loc) (toTree inv : toTree bound : toForest guard)

   toTree (Skip loc _) = leaf $ "Skip "++ showL loc
       
   toTree (States sc loc exps _) = 
      Node (show sc ++ " " ++ showL loc) [toTree exps]

   toTree (String loc value _) = 
      leaf ("String: `" ++ value ++ "` " ++ showL loc)
      
   toTree (Unary op loc expr _) = 
      Node (show op ++ " " ++ showL loc) [toTree expr]

   toTree (Write ln expr loc _) =
      Node tWrite [toTree expr]
      where tWrite = (if ln then "WriteLn " else "Write ") ++ showL loc

   toTree e = Node "You're my Creator but you never teach me how to draw myself as a Tree :''(" 
               [ leaf (show e)]



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

putLocation :: Location -> String
putLocation location = " --- en el " <> show location


putLocationLn :: Location -> String
putLocationLn location = " --- en el " <> show location <> "\n"


drawAST :: Show a => Int -> AST a -> String
drawAST level (Program name loc defs accs ast) =
   putSpacesLn level <> "Programa: " <> show name <> putLocation loc
                     <> " //Tag: "   <> show ast
                     <> drawASTList (level + 4) defs <> drawAST (level + 4) accs


drawAST level (DefProc name _ accs pre post bound _ _ ast) =
   putSpacesLn level <> "Procedimiento: " <> show name  -- <> putLocation loc
                     <> " //Tag: "        <> show ast
                     <> putSpaces (level + 4)   <> drawAST (level + 4) pre
                     <> putSpaces (level + 4)   <> drawAST (level + 8) bound
                     <> putSpacesLn (level + 4) <> "Acciones: " <> drawAST (level + 8) accs
                     <> putSpaces (level + 4)   <> drawAST (level + 4) post


drawAST level (States t loc expr ast) =
   putSpacesLn level <> show t     <> putLocation loc
                     <> " //Tag: " <> show ast
                     <> drawAST (level + 4) expr


drawAST level (Arithmetic t loc lexpr rexp ast) =
   putSpacesLn   level <> "Operador Aritmético: " <> show t <> putLocation loc
                       <> " //Tag: "              <> show ast
                       <> putSpacesLn (level + 4) <> "Lado izquierdo:"
                                                         <> drawAST (level + 8) lexpr
                       <> putSpacesLn (level + 4) <> "Lado derecho:"
                                                         <> drawAST (level + 8) rexp


drawAST level (Relational t loc lexpr rexp ast) =
   putSpacesLn level <> "Operador Relacional: " <> show t <> putLocation loc
                     <> " //Tag: "              <> show ast
                     <> putSpacesLn (level + 4) <> "Lado izquierdo:"
                                                       <> drawAST (level + 8) lexpr
                     <> putSpacesLn (level + 4) <> "Lado derecho:"
                                                       <> drawAST (level + 8) rexp


drawAST level (Boolean t loc lexpr rexp ast) =
   putSpacesLn level <> "Operador Booleano: "   <> show t <> putLocation loc
                     <> " //Tag: "              <> show ast
                     <> putSpacesLn (level + 4) <> "Lado izquierdo:"
                                                       <> drawAST (level + 8) lexpr
                     <> putSpacesLn (level + 4) <> "Lado derecho:"
                                                       <> drawAST (level + 8) rexp


drawAST level (Id loc cont ast) =
   putSpacesLn level <> "Id: "        <> show cont <> putLocation loc
                     <> " //Tag: "    <> show ast


drawAST level (Int loc cont ast) =
   putSpacesLn level <> "Entero: "    <> show cont <> putLocation loc
                     <> " //Tag: "    <> show ast


drawAST level (Float loc cont ast) =
   putSpacesLn level <> "Flotante: "  <> show cont <> putLocation loc
                     <> " //Tag: "    <> show ast


drawAST level (Bool loc cont ast) =
   putSpacesLn level <> "Booleano: "  <> show cont <> putLocation loc
                     <> " //Tag: "    <> show ast


drawAST level (Char loc cont ast) =
   putSpacesLn level <> "Caracter: "  <> show cont <> putLocation loc
                     <> " //Tag: "    <> show ast


drawAST level (String loc cont ast) =
   putSpacesLn level <> "String: "    <> show cont <> putLocation loc
                     <> " //Tag: "    <> show ast


drawAST level (Constant loc t max ast) =
   putSpacesLn level <> "Constante: " <> checkMaxMin t max <> putLocation loc
                     <> " //Tag: "    <> show ast


drawAST level (LAssign idlist explist loc ast) =
   putSpacesLn level <> "Asignación: " <> putLocation loc
                     <> " //Tag: "     <> show ast
                     <> drawLAssign (level) idlist explist


drawAST level ((Rept guard inv bound loc ast)) =
   putSpacesLn level <> "Repetición: " <> putLocation loc
                     <> " //Tag: "     <> show ast
                     <> drawASTList (level + 4) guard
                     <> putSpaces   (level + 4) <> drawAST (level + 4) inv
                     <> putSpaces   (level + 4) <> drawAST (level + 4) bound


drawAST level ((Cond guard loc ast)) =
   putSpacesLn level <> "Condicional: " <> putLocation loc
                     <> " //Tag: "      <> show ast
                     <> drawASTList (level + 4) guard


drawAST level ((Guard exp action loc ast)) =
   putSpacesLn level <> "Guardia: " <> putLocation loc
                     <> " //Tag: "  <> show ast
                     <> drawAST (level + 4) exp
                     <> putSpacesLn (level + 4) <> "Acciones:"
                     <> drawAST (level + 8) action


drawAST level ((GuardExp exp action loc ast)) =
   putSpacesLn level <> "Guardia de Expresión: " <> putLocation loc
                     <> " //Tag: "               <> show ast
                     <> drawAST (level + 4) exp
                     <> putSpacesLn (level + 4) <> "Expresión:"
                     <> drawAST (level + 8) action


drawAST level ((Block loc st _ action ast)) =
   putSpacesLn level <> "Bloque: " <> putLocation loc
                     <> " //Tag: " <> show ast  <> show st
                     <> drawASTList (level + 4) action


drawAST level ((Skip loc ast)) =
   putSpacesLn level <> "Saltar: " <> putLocation loc
                     <> " //Tag: " <> show ast


drawAST level ((Abort loc ast)) =
   putSpacesLn level <> "Abortar: " <> putLocation loc
                     <> " //Tag: "  <> show ast


drawAST level ((Ran var _ loc ast)) =
   putSpacesLn level <> "Aleatorio: " <> show var <> putLocation loc
                     <> " //Tag: "    <> show ast


drawAST level ((Write ln exp loc ast)) =
   putSpacesLn level <> checkWrite ln <> putLocation loc
                     <> " //Tag: "  <> show ast
                     <> drawAST (level + 4) (exp)


drawAST level ((GuardAction loc assert action  ast)) =
   putSpacesLn level <> "Guardia de Acción: " <> putLocation loc
                     <> " //Tag: "            <> show ast
                     <> drawAST (level + 4) assert
                     <> putSpacesLn (level + 4) <> "Acciones:"
                     <> drawAST (level + 8) action



drawAST level ((Quant op var loc range term ast)) =
   putSpacesLn level <> "Cuantificador: " <> show op <> putLocation loc
                     <> " //Tag: "        <> show ast
   <> putSpacesLn (level + 4) <> "Variable cuantificada: " <> show var
   <> putSpacesLn (level + 4) <> "Rango: "  <> drawAST (level + 8) range
   <> putSpacesLn (level + 4) <> "Cuerpo: " <> drawAST (level + 8) term



drawAST level ((Conversion t loc exp ast)) =
   putSpacesLn level <> "Conversión: " <> show t <> putLocation loc
                     <> " //Tag: "     <> show ast
                     <> drawAST (level + 4) exp



drawAST level ((Unary op loc exp ast)) =
   putSpacesLn level <> show op    <> putLocation loc
                     <> " //Tag: " <> show ast
                     <> drawAST (level + 4) exp



drawAST level ((FCallExp name _ loc args ast)) =
   putSpacesLn level <> "Llamada de la Función: " <> show name <> putLocation loc
                     <> " //Tag: "                <> show ast
   <> putSpacesLn (level + 4) <> "Argumentos: " <> drawASTList (level + 8) args



drawAST level ((ArrCall loc name args ast)) =
   putSpacesLn level <> "Llamada del Arreglo: " <> show name <> putLocation loc
                     <> " //Tag: "              <> show ast
   <> putSpacesLn (level + 4) <> "Argumentos: " <> drawASTList (level + 8) args



drawAST level ((ProcCall name st loc args ast)) =
   putSpacesLn level <> "Llamada del Procedimiento: " <> show name <> putLocation loc
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
