module AST where
--------------------------------------------------------------------------------
-- import           Limits
import           Location
import           SymbolTable
import           Type
--------------------------------------------------------------------------------
import           Contents         (Contents)
import           Data.Monoid      ((<>))
import           Data.Range.Range (Range)
import           Data.Text        (Text)
--------------------------------------------------------------------------------

{- |
   Tipo de dato que nos permite representar el árbol sintáctico abstracto
   del lenguaje. Los campos @line@ y @column@ representan la línea y columna, respectivamente,
   del nodo en el texto del programa.
 -}

data OpNum = Sum | Sub | Mul | Div | Exp | Max | Min | Mod
      deriving (Eq)

instance Show OpNum where
   show Sum = "Suma"
   show Sub = "Resta"
   show Mul = "Multiplicación"
   show Div = "División"
   show Exp = "Potencia"
   show Max = "Máximo"
   show Min = "Mínimo"
   show Mod = "Módulo"


data OpBool = Dis | Con | Implies | Conse
      deriving (Eq)

instance Show OpBool where
   show Dis     = "Disjunción"
   show Con     = "Conjunción"
   show Implies = "Implicación"
   show Conse   = "Consecuencia"


data OpRel = Equ | Less | Greater | LEqual | GEqual | Ine
      deriving (Eq)

instance Show OpRel where
   show Equ     = "Equivalencia"
   show Less    = "Menor que"
   show Greater = "Mayor que"
   show LEqual  = "Menor o igual que"
   show GEqual  = "Mayor o igual que"
   show Ine     = "Negación"


data Conv = ToInt | ToDouble | ToChar
      deriving (Eq)

instance Show Conv where
   show ToInt    = "a int"
   show ToDouble = "a double"
   show ToChar   = "a char"


data OpUn = Minus | Not | Abs | Sqrt
      deriving (Eq)

instance Show OpUn where
   show Minus  = "Negativo"
   show Not    = "Negación"
   show Abs    = "Valor Absoluto"
   show Sqrt   = "Raíz Cuadrada"


data StateCond = Pre | Post | Assertion | Bound | Invariant
      deriving (Eq)

instance Show StateCond where
   show Pre       = "Precondición"
   show Post      = "Postcondición"
   show Assertion = "Aserción"
   show Bound     = "Función de Cota"
   show Invariant = "Invariante"


data OpQuant = ForAll | Exists | Summation | Product | Minimum | Maximum
    deriving(Eq)

instance Show OpQuant where
   show ForAll    = "Universal (forall)"
   show Exists    = "Existencial (exist)"
   show Summation = "Sumatoria (sigma)"
   show Product   = "Productoria (pi)"
   show Minimum   = "Minimo (min)"
   show Maximum   = "Maximo (max)"


--Rangos
data OpSet = Union | Intersec
      deriving (Eq)

data UnknownRange = SetRange   { getOp :: OpSet, getLexp :: UnknownRange, getRexp :: UnknownRange }
                  | TupleRange { getLeft :: AST Type, getRight :: AST Type }
      deriving (Eq)


data AST a = Arithmetic { opBinA   :: OpNum   , location :: Location, lexpr :: AST a, rexp :: AST a, tag :: a      } -- ^ Operadores Matematicos de dos expresiones.
         | Boolean      { opBinB   :: OpBool  , location :: Location, lexpr :: AST a, rexp :: AST a, tag :: a      } -- ^ Operadores Booleanos de dos expresiones.
         | Relational   { opBinR   :: OpRel   , location :: Location, lexpr :: AST a, rexp :: AST a, tag :: a      } -- ^ Operadores Relacionales de dos expresiones.
         | ArrCall      { location :: Location, name     :: Text, list :: [AST a],        tag :: a                   } -- ^ Búsqueda en arreglo.
         | ID           { location :: Location, id       :: Text, tag :: a                                           } -- ^ Identificador.
         | Int          { location :: Location, expInt   :: Integer, tag :: a                                          } -- ^ Numero entero.
         | Float        { location :: Location, expFloat :: Double, tag :: a                                           } -- ^ Numero entero.
         | Bool         { location :: Location, cbool    :: Bool, tag :: a                                             } -- ^ Tipo booleano con el token.
         | Char         { location :: Location, mchar    :: Char, tag :: a                                             } -- ^ Tipo caracter con el token.
         | String       { location :: Location, mstring  :: String, tag :: a                                           } -- ^ Tipo string con el token.
         | Constant     { location :: Location, int      :: Bool    , max    :: Bool,    tag :: a                      } -- ^ Constantes.
         | Conversion   { toType   :: Conv    , location :: Location, tiexp  :: AST a, tag :: a                      } -- ^ Conversión a entero.
         | Unary        { opUn     :: OpUn    , location :: Location, lenExp :: AST a, tag :: a                      } -- ^ Función raíz cuadrada.
         | Skip         { location :: Location, tag :: a                                                               } -- ^ Instruccion Skip.
         | Abort        { location :: Location, tag :: a                                                               } -- ^ Instruccion Abort.
         | Cond         { cguard   :: [AST a], location :: Location, tag :: a                                          } -- ^ Instruccion If.
         | Block        { location :: Location, blockStable :: SymbolTable, listDec :: [AST a], lisAct   :: [AST a]
                        , tag :: a                                                                                     }
         | Rept         { rguard   :: [AST a], rinv   :: AST a, rbound   :: AST a, location ::Location, tag :: a   } -- ^ Instruccion Do.
         | ConsAssign   { location  :: Location, caID :: [(Text, Location)], caExpr :: [AST a], tag :: a             }

         | LAssign      { idlist   :: [AST a], explista :: [AST a], location :: Location, tag :: a                     } -- ^

         | Write        { ln       :: Bool , wexp     :: AST a, location :: Location, tag :: a                       } -- ^ Escribir.
         | FCallExp     { fname    :: Text, astSTable :: SymbolTable, location :: Location, args :: [AST a], tag :: a} -- ^ Llamada a funcion.
         | ProcCall     { pname    :: Text, astSTable :: SymbolTable, location  :: Location
                        , args     :: [AST a], tag :: a                                                                }
         | ProcCallCont { pname    :: Text, astSTable :: SymbolTable, location  :: Location
                        , args     :: [AST a], con :: Contents SymbolTable, tag :: a                                           }
         | DecArray     { dimension :: [AST a], tag :: a                                                               }
         | Guard        { gexp     :: AST a, gact   ::  AST a, location :: Location, tag :: a                      } -- ^ Guardia.
         | GuardExp     { gexp     :: AST a, gact   ::  AST a, location :: Location, tag :: a                      } -- ^ Guardia de Expresion.
         | DefFun       { dfname   :: Text, astSTable :: SymbolTable, location :: Location, fbody    ::  AST a
                        , retType  :: Type, nodeBound :: AST a, params :: [(Text, Type)], tag :: a }
         | DefProc      { pname     :: Text, astSTable :: SymbolTable, prbody    :: AST a, nodePre   :: AST a
                        , nodePost  :: AST a, nodeBound :: AST a, constDec  :: [AST a], params :: [(Text, Type)]
                        , tag       :: a
                        }
         | Ran          { var      :: Text, retType :: Type, location :: Location, tag :: a                                           }
         | Program      { pname    :: Text, location  :: Location, listdef :: [AST a],  listacc :: AST a, tag :: a }
         | GuardAction  { location :: Location , assertionGa :: AST a, actionGa :: AST a, tag :: a                 }
         | States       { tstate   :: StateCond, location :: Location,   exps     :: AST a, tag :: a                 }
         | Quant        { opQ      :: OpQuant, varQ :: Text, location :: Location, rangeExp :: AST a
                         ,termExpr :: AST a, tag :: a                                                                }
         | QuantRan     { opQ      :: OpQuant, varQ :: Text, location :: Location, rangeVExp :: [Range Integer]
                         ,termExpr :: AST a, tag :: a                                                                }
         | QuantRanUn   { opQ      :: OpQuant, varQ :: Text, location :: Location, rangeUExp :: UnknownRange
                         ,termExpr :: AST a, tag :: a                                                                }
         | EmptyRange   { location :: Location, tag :: a                                                               }
         | EmptyAST     { tag :: a                                                                                     }
         | Read         { location :: Location, file :: Maybe String, varTypes :: [Type], vars :: [(Text, Location)], tag :: a                       }
    deriving (Eq)


astToId :: AST a -> Maybe Text
astToId (ID _ id _) = Just id
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


drawAST level (ID loc cont ast) =
   putSpacesLn level <> "ID: "        <> show cont <> putLocation loc
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


drawASTList level xs = foldl (\acc d -> (acc <> drawAST level (d))) [] xs


drawLAssign level idlist explist = foldl (\acc (id, exp) ->
   (acc <> putSpacesLn (level + 4) <> "Variable: " <> show (id)
        <> putSpacesLn (level + 8) <> "Lado derecho: "
        <> drawAST (level + 12) exp)) [] $ zip idlist explist
