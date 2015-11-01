
module AST where

import qualified Data.Text as T
import Data.Range.Range    as RA
import Data.Monoid hiding (Product)
import SymbolTable
import Location
import Print
import Type

{- |
   Tipo de dato que nos permite representar el árbol sintáctico abstracto
   del lenguaje. Los campos @line@ y @column@ representan la línea y columna, respectivamente,
   del nodo en el texto del programa.
 -}

data OpNum = Sum | Sub | Mul | Div | Exp | Max | Min | Mod
      deriving (Eq)

instance Show OpNum where
   show AST.Sum = "Suma"
   show Sub     = "Resta"
   show Mul     = "Multiplicación"
   show Div     = "División"
   show Exp     = "Potencia"
   show Max     = "Máximo"
   show Min     = "Mínimo"
   show Mod     = "Módulo"


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
   show Minus  = "Negativo: "
   show Not    = "Negación: "
   show Abs    = "Valor Absoluto: "
   show Sqrt   = "Raíz Cuadrada: "


data StateCond = Pre | Post | Assertion | Bound | Invariant
      deriving (Eq)

instance Show StateCond where
   show Pre       = "Precondición "
   show Post      = "Postcondición "
   show Assertion = "Aserción "
   show Bound     = "Función de Cota "
   show Invariant = "Invariante "


data OpQuant = ForAll | Exists | Summation | Product | Minimum | Maximum
    deriving(Eq)

instance Show OpQuant where
   show ForAll    = "Universal (forall)"
   show Exists    = "Existencial (exist)"
   show Summation = "Sumatoria (sigma)"
   show Product   = "Productoria (pi)"
   show Minimum   = "Minimo (min)"
   show Maximum   = "Maximo (max)"


data AST a = Arithmetic { opBinA   :: OpNum   , location :: Location, lexpr :: (AST a), rexp :: (AST a), tag :: a      } -- ^ Operadores Matematicos de dos expresiones.
         | Boolean      { opBinB   :: OpBool  , location :: Location, lexpr :: (AST a), rexp :: (AST a), tag :: a      } -- ^ Operadores Booleanos de dos expresiones.
         | Relational   { opBinR   :: OpRel   , location :: Location, lexpr :: (AST a), rexp :: (AST a), tag :: a      } -- ^ Operadores Relacionales de dos expresiones.     
         | ArrCall      { location :: Location, name     :: T.Text, list :: [AST a],        tag :: a                   } -- ^ Búsqueda en arreglo.
         | ID           { location :: Location, id       :: T.Text, tag :: a                                           } -- ^ Identificador.
         | Int          { location :: Location, expInt   :: Integer, tag :: a                                          } -- ^ Numero entero.
         | Float        { location :: Location, expFloat :: Double, tag :: a                                           } -- ^ Numero entero.
         | Bool         { location :: Location, cbool    :: Bool, tag :: a                                             } -- ^ Tipo booleano con el token.
         | Char         { location :: Location, mchar    :: Char, tag :: a                                             } -- ^ Tipo caracter con el token. 
         | String       { location :: Location, mstring  :: String, tag :: a                                           } -- ^ Tipo string con el token.
         | Constant     { location :: Location, int      :: Bool    , max    :: Bool,    tag :: a                      } -- ^ Constantes.   
         | Convertion   { toType   :: Conv    , location :: Location, tiexp  :: (AST a), tag :: a                      } -- ^ Conversión a entero.
         | Unary        { opUn     :: OpUn    , location :: Location, lenExp :: (AST a), tag :: a                      } -- ^ Función raíz cuadrada.
         | Skip         { location :: Location, tag :: a                                                               } -- ^ Instruccion Skip.
         | Abort        { location :: Location, tag :: a                                                               } -- ^ Instruccion Abort.
         | Cond         { cguard   :: [AST a], location :: Location, tag :: a                                          } -- ^ Instruccion If.
         | Block        { location :: Location, blockStable :: SymbolTable, listDec :: [AST a], lisAct   :: [AST a]
                        , tag :: a                                                                                     }
         | Rept         { rguard   :: [AST a], rinv   :: (AST a), rbound   :: (AST a), location ::Location, tag :: a   } -- ^ Instruccion Do.
         | ConsAssign   { location  :: Location, caID :: [(T.Text, Location)], caExpr :: [AST a], tag :: a             }

         | LAssign      { idlist   :: [AST a], explista :: [AST a], location :: Location, tag :: a                     } -- ^
         
         | Write        { ln       :: Bool , wexp     :: (AST a), location :: Location, tag :: a                       } -- ^ Escribir.
         | FCallExp     { fname    :: T.Text, astSTable :: SymbolTable, location :: Location, args :: [AST a], tag :: a} -- ^ Llamada a funcion.
         | ProcCall     { pname    :: T.Text, astSTable :: SymbolTable, location  :: Location
                        , args     :: [AST a], tag :: a                                                                } 
         | DecArray     { dimension :: [AST a], tag :: a                                                               }
         | Guard        { gexp     :: (AST a), gact   ::  (AST a), location :: Location, tag :: a                      } -- ^ Guardia.
         | GuardExp     { gexp     :: (AST a), gact   ::  (AST a), location :: Location, tag :: a                      } -- ^ Guardia de Expresion.
         | DefFun       { dfname   :: T.Text, astSTable :: SymbolTable, location :: Location, fbody    ::  (AST a)
                        , retType  :: Type, nodeBound :: (AST a), params :: [(T.Text, Type)], tag :: a }
         | DefProc      { pname     :: T.Text, astSTable :: SymbolTable, prbody    :: [AST a], nodePre   :: (AST a)
                        , nodePost  :: (AST a), nodeBound :: (AST a), constDec  :: [AST a], params :: [(T.Text, Type)]
                        , tag       :: a
                        }
         | Ran          { var      :: T.Text, retType :: Type, location :: Location, tag :: a                                           }
         | Program      { pname    :: T.Text, location  :: Location, listdef :: [AST a],  listacc :: [AST a], tag :: a }
         | GuardAction  { location :: Location , assertionGa :: (AST a), actionGa :: (AST a), tag :: a                 }
         | States       { tstate   :: StateCond, location :: Location,   exps     :: (AST a), tag :: a                 }
         | Quant        { opQ      :: OpQuant, varQ :: T.Text, location :: Location, rangeExp :: (AST a)
                         ,termExpr :: (AST a), tag :: a                                                                }
         | QuantRan     { opQ      :: OpQuant, varQ :: T.Text, location :: Location, rangeVExp :: [RA.Range Integer]
                         ,termExpr :: (AST a), tag :: a                                                                }
         | EmptyRange   { location :: Location, tag :: a                                                               }
         | EmptyAST     { tag :: a                                                                                     }
         | Read         { location :: Location, file :: Maybe String, varTypes :: [Type], vars :: [(T.Text, Location)], tag :: a                       }
    deriving (Eq)


astToId :: AST a -> Maybe T.Text
astToId (ID _ id _) = Just id
astToId (ArrCall _ id _ _) = Just id
astToId _           = Nothing

instance Show a => Show (AST a) where
  show ast = drawAST 0 ast


checkMaxMin :: Bool -> Bool -> String
checkMaxMin True  True  = "MAX_INT"
checkMaxMin True  False = "MIN_INT"
checkMaxMin False True  = "MAX_DOUBLE"
checkMaxMin False False = "MIN_DOUBLE"


checkWrite :: Bool -> String
checkWrite True  = "Escribir con Salto de línea"
checkWrite False = "Escribir"



drawAST level ((Program name loc defs accs ast)) = 
   putSpacesLn level `mappend` "Programa: " `mappend` show name `mappend` putLocation loc 
                     `mappend` " //Tag: "   `mappend` show ast 
                     `mappend` drawASTList (level + 4) defs     `mappend` drawASTList (level + 4) accs


drawAST level ((DefProc name _ accs pre post bound _ _ ast)) = 
   putSpacesLn level `mappend` "Procedimiento: " `mappend` show name  -- `mappend` putLocation loc
                     `mappend` " //Tag: "        `mappend` show ast  
                     `mappend` putSpaces (level + 4)   `mappend` drawAST (level + 4) pre      
                     `mappend` putSpaces (level + 4)   `mappend` drawAST (level + 8) bound  
                     `mappend` putSpacesLn (level + 4) `mappend` "Acciones: " `mappend` drawASTList (level + 8) accs
                     `mappend` putSpaces (level + 4)   `mappend` drawAST (level + 4) post 


drawAST level ((States t loc expr ast)) = 
   putSpacesLn level `mappend` show t     `mappend` putLocation loc 
                     `mappend` " //Tag: " `mappend` show ast       
                     `mappend` drawAST (level + 4) expr


drawAST level ((Arithmetic t loc lexpr rexp ast)) =
   putSpacesLn   level `mappend` "Operador Aritmético: " `mappend` show t `mappend` putLocation loc 
                       `mappend` " //Tag: "              `mappend` show ast 
                       `mappend` putSpacesLn (level + 4) `mappend` "Lado izquierdo:" 
                                                         `mappend` drawAST (level + 8) lexpr 
                       `mappend` putSpacesLn (level + 4) `mappend` "Lado derecho:"  
                                                         `mappend` drawAST (level + 8) rexp       
  

drawAST level ((Relational t loc lexpr rexp ast)) =
   putSpacesLn level `mappend` "Operador Relacional: " `mappend` show t `mappend` putLocation loc 
                     `mappend` " //Tag: "              `mappend` show ast 
                     `mappend` putSpacesLn (level + 4) `mappend` "Lado izquierdo:" 
                                                       `mappend` drawAST (level + 8) lexpr 
                     `mappend` putSpacesLn (level + 4) `mappend` "Lado derecho:"  
                                                       `mappend` drawAST (level + 8) rexp 
                         

drawAST level ((Boolean t loc lexpr rexp ast)) =
   putSpacesLn level `mappend` "Operador Booleano: "   `mappend` show t `mappend` putLocation loc 
                     `mappend` " //Tag: "              `mappend` show ast 
                     `mappend` putSpacesLn (level + 4) `mappend` "Lado izquierdo:" 
                                                       `mappend` drawAST (level + 8) lexpr
                     `mappend` putSpacesLn (level + 4) `mappend` "Lado derecho:"  
                                                       `mappend` drawAST (level + 8) rexp 
                  

drawAST level ((ID loc cont ast)) =
   putSpacesLn level `mappend` "ID: "        `mappend` show cont `mappend` putLocation loc 
                     `mappend` " //Tag: "    `mappend` show ast 


drawAST level ((Int loc cont ast)) =
   putSpacesLn level `mappend` "Entero: "    `mappend` show cont `mappend` putLocation loc 
                     `mappend` " //Tag: "    `mappend` show ast 


drawAST level ((Float loc cont ast)) =
   putSpacesLn level `mappend` "Flotante: "  `mappend` show cont `mappend` putLocation loc 
                     `mappend` " //Tag: "    `mappend` show ast 


drawAST level ((Bool loc cont ast)) =
   putSpacesLn level `mappend` "Booleano: "  `mappend` show cont `mappend` putLocation loc 
                     `mappend` " //Tag: "    `mappend` show ast 


drawAST level ((Char loc cont ast)) =
   putSpacesLn level `mappend` "Caracter: "  `mappend` show cont `mappend` putLocation loc 
                     `mappend` " //Tag: "    `mappend` show ast 


drawAST level ((String loc cont ast)) =
   putSpacesLn level `mappend` "String: "    `mappend` show cont `mappend` putLocation loc 
                     `mappend` " //Tag: "    `mappend` show ast 


drawAST level ((Constant loc t max ast)) =
   putSpacesLn level `mappend` "Constante: " `mappend` checkMaxMin t max `mappend` putLocation loc 
                     `mappend` " //Tag: "    `mappend` show ast 


drawAST level ((LAssign idlist explist loc ast)) =
   putSpacesLn level `mappend` "Asignación: " `mappend` putLocation loc
                     `mappend` " //Tag: "     `mappend` show ast  
                     `mappend` drawLAssign (level) idlist explist


drawAST level ((Rept guard inv bound loc ast)) =
   putSpacesLn level `mappend` "Repetición: " `mappend` putLocation loc 
                     `mappend` " //Tag: "     `mappend` show ast 
                     `mappend` drawASTList (level + 4) guard
                     `mappend` putSpaces   (level + 4) `mappend` drawAST (level + 4) inv   
                     `mappend` putSpaces   (level + 4) `mappend` drawAST (level + 4) bound 


drawAST level ((Cond guard loc ast)) =
   putSpacesLn level `mappend` "Condicional: " `mappend` putLocation loc 
                     `mappend` " //Tag: "      `mappend` show ast 
                     `mappend` drawASTList (level + 4) guard


drawAST level ((Guard exp action loc ast)) =
   putSpacesLn level `mappend` "Guardia: " `mappend` putLocation loc
                     `mappend` " //Tag: "  `mappend` show ast  
                     `mappend` drawAST (level + 4) exp 
                     `mappend` putSpacesLn (level + 4) `mappend` "Acciones:"  
                     `mappend` drawAST (level + 8) action


drawAST level ((GuardExp exp action loc ast)) =
   putSpacesLn level `mappend` "Guardia de Expresión: " `mappend` putLocation loc 
                     `mappend` " //Tag: "               `mappend` show ast 
                     `mappend` drawAST (level + 4) exp 
                     `mappend` putSpacesLn (level + 4) `mappend` "Expresión:"  
                     `mappend` drawAST (level + 8) action


drawAST level ((Block loc st _ action ast)) =
   putSpacesLn level `mappend` "Bloque: " `mappend` putLocation loc
                     `mappend` " //Tag: " `mappend` show ast  `mappend` show st
                     `mappend` drawASTList (level + 4) action


drawAST level ((Skip loc ast)) =
   putSpacesLn level `mappend` "Saltar: " `mappend` putLocation loc 
                     `mappend` " //Tag: " `mappend` show ast 


drawAST level ((Abort loc ast)) =
   putSpacesLn level `mappend` "Abortar: " `mappend` putLocation loc 
                     `mappend` " //Tag: "  `mappend` show ast 


drawAST level ((Ran var _ loc ast)) =
   putSpacesLn level `mappend` "Aleatorio: " `mappend` show var `mappend` putLocation loc 
                     `mappend` " //Tag: "    `mappend` show ast 
  

drawAST level ((Write ln exp loc ast)) =
   putSpacesLn level `mappend` checkWrite ln `mappend` putLocation loc 
                     `mappend` " //Tag: "  `mappend` show ast 
                     `mappend` drawAST (level + 4) (exp)


drawAST level ((GuardAction loc assert action  ast)) =
   putSpacesLn level `mappend` "Guardia de Acción: " `mappend` putLocation loc 
                     `mappend` " //Tag: "            `mappend` show ast 
                     `mappend` drawAST (level + 4) assert
                     `mappend` putSpacesLn (level + 4) `mappend` "Acciones:"  
                     `mappend` drawAST (level + 8) action



drawAST level ((Quant op var loc range term ast)) =
   putSpacesLn level `mappend` "Cuantificador: " `mappend` show op `mappend` putLocation loc 
                     `mappend` " //Tag: "        `mappend` show ast 
   `mappend` putSpacesLn (level + 4) `mappend` "Variable cuantificada: " `mappend` show var          
   `mappend` putSpacesLn (level + 4) `mappend` "Rango: "  `mappend` drawAST (level + 8) range 
   `mappend` putSpacesLn (level + 4) `mappend` "Cuerpo: " `mappend` drawAST (level + 8) term   



drawAST level ((Convertion t loc exp ast)) =
   putSpacesLn level `mappend` "Conversión: " `mappend` show t `mappend` putLocation loc 
                     `mappend` " //Tag: "     `mappend` show ast 
                     `mappend` drawAST (level + 4) exp  



drawAST level ((Unary op loc exp ast)) =
   putSpacesLn level `mappend` show op    `mappend` putLocation loc 
                     `mappend` " //Tag: " `mappend` show ast 
                     `mappend` drawAST (level + 4) exp 



drawAST level ((FCallExp name _ loc args ast)) =
   putSpacesLn level `mappend` "Llamada de la Función: " `mappend` show name `mappend` putLocation loc 
                     `mappend` " //Tag: "                `mappend` show ast 
   `mappend` putSpacesLn (level + 4) `mappend` "Argumentos: " `mappend` drawASTList (level + 8) args 



drawAST level ((ArrCall loc name args ast)) =
   putSpacesLn level `mappend` "Llamada del Arreglo: " `mappend` show name `mappend` putLocation loc 
                     `mappend` " //Tag: "              `mappend` show ast 
   `mappend` putSpacesLn (level + 4) `mappend` "Argumentos: " `mappend` drawASTList (level + 8) args 

                         

drawAST level ((ProcCall name _ loc args ast)) =
   putSpacesLn level `mappend` "Llamada del Procedimiento: " `mappend` show name `mappend` putLocation loc 
                     `mappend` " //Tag: "                    `mappend` show ast 
   `mappend` putSpacesLn (level + 4) `mappend` "Argumentos: " `mappend` drawASTList (level + 8) args 


drawAST level ((DefFun name st _ body _ bound _ ast)) =
   putSpacesLn level `mappend` "Función: " `mappend` show name
                     `mappend` " //Tag: "   `mappend` show ast 
                     `mappend` drawAST(level + 4) bound   
                     `mappend` drawAST(level + 4) body  


                     --`mappend` "\n\n\n iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii \n\n"  
                     --`mappend` show (fromJust $ getPadre st)
                     --`mappend` "\n\n\n ffffffffffffffffffffffffffffffffffffffff \n\n"  


drawAST _ (EmptyRange _ _) = "rango vacio"
drawAST _ (EmptyAST _) = "vacio"



drawASTList level xs = foldl (\acc d -> (acc `mappend` drawAST level (d))) [] xs


drawLAssign level idlist explist = foldl (\acc (id, exp) -> 
   (acc `mappend` putSpacesLn (level + 4) `mappend` "Variable: " `mappend` show (id) 
        `mappend` putSpacesLn (level + 8) `mappend` "Lado derecho: "
        `mappend` drawAST (level + 12) (exp))) [] $ zip idlist explist
