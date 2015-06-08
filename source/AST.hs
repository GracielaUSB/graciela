module AST where

import qualified Data.Text as T
import Data.Monoid
import Location
import Token
import Data.Range.Range                   as RA
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
   show Dis     = "Disyunción"
   show Con     = "Conjunción"
   show Implies = "Implicación"
   show Conse   = "Consecuencia"

data OpRel = Equ | Less | Greater | LEqual | GEqual | Ine | Equal
      deriving (Eq) 

instance Show OpRel where
   show Equ     = "Equivalencia"
   show Less    = "Menor que"
   show Greater = "Mayor que"
   show LEqual  = "Menor o igual que"
   show GEqual  = "Mayor o igual que"
   show Ine     = "Negación"
   show Equal   = "Inequivalencia"



data Conv = ToInt | ToDouble | ToString | ToChar  
      deriving (Eq) 

instance Show Conv where
   show ToInt    = "a Entero"
   show ToDouble = "a Flotante"
   show ToString = "a Cadenas de Caracteres"
   show ToChar   = "a Caracter"



data OpUn = Minus | Not | Abs | Sqrt | Length  
      deriving (Eq) 

instance Show OpUn where
   show Minus  = "Negativo: "
   show Not    = "Negación: "
   show Abs    = "Valor Absoluto: "
   show Sqrt   = "Raíz Cuadrada: "
   show Length = "Longitud: "


data StateCond = Pre | Post | Assertion | Bound | Invariant
      deriving (Eq)

instance Show StateCond where
   show Pre       = "Precondición "
   show Post      = "Postcondición "
   show Assertion = "Aserción "
   show Bound     = "Función de Cota "
   show Invariant = "Invariante "



data AST a = Arithmetic { opBinA   :: OpNum   , location :: Location, lexpr :: (AST a), rexp :: (AST a), tag :: a      } -- ^ Operadores Matematicos de dos expresiones.
         | Boolean      { opBinB   :: OpBool  , location :: Location, lexpr :: (AST a), rexp :: (AST a), tag :: a      } -- ^ Operadores Booleanos de dos expresiones.
         | Relational   { opBinR   :: OpRel   , location :: Location, lexpr :: (AST a), rexp :: (AST a), tag :: a      } -- ^ Operadores Relacionales de dos expresiones.     
         | FCallExp     { location :: Location, fname    :: T.Text, args     :: [AST a], tag :: a                      } -- ^ Llamada a funcion.
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
         | Block        { lisAct   :: [AST a], location :: Location, tag :: a                                          }
         | Rept         { rguard   :: [AST a], rinv   :: (AST a), rbound   :: (AST a), location ::Location, tag :: a   } -- ^ Instruccion Do.
         | LAssign      { idlist   :: [((T.Text, Type), [AST a])], explista :: [AST a], location :: Location, tag :: a } -- ^
         | Write        { ln       :: Bool , wexp     :: (AST a), location :: Location, tag :: a                       } -- ^ Escribir.
         | ProcCall     { pname    :: T.Text, args     :: [AST a], location :: Location, tag :: a                      } -- ^ Llamada a funcion.
         | Guard        { gexp     :: (AST a), gact   ::  (AST a), location :: Location, tag :: a                      } -- ^ Guardia.
         | GuardExp     { gexp     :: (AST a), gact   ::  (AST a), location :: Location, tag :: a                      } -- ^ Guardia de Expresion.
         | DefFun       { dfname   :: T.Text, location :: Location, fbody    ::  (AST a), nodeBound :: (AST a), tag :: a }
         | DefProc      { pname    :: T.Text, prbody   ::  [AST a], nodePre   :: (AST a)
                         ,nodePost :: (AST a), nodeBound :: (AST a), tag    :: a                                       }
         | Ran          { var      :: T.Text, location :: Location, tag :: a                                           }
         | Program      { pname    :: T.Text, location  :: Location, listdef :: [AST a],  listacc :: [AST a], tag :: a }
         | FunBody      { location :: Location , fbexpr      :: (AST a), tag :: a                                      }
         | GuardAction  { location :: Location , assertionGa :: (AST a), actionGa :: (AST a), tag :: a                 }
         | States       { tstate   :: StateCond, location :: Location,   exps     :: (AST a), tag :: a                 }
         | Quant        { opQ      :: Token, varQ :: T.Text, location :: Location, rangeExp :: (AST a)
                         ,termExpr :: (AST a), tag :: a                                                                }
         | QuantRan     { opQ      :: Token, varQ :: T.Text, location :: Location, rangeVExp :: RA.Range Integer
                         ,termExpr :: (AST a), tag :: a                                                                }
         | EmptyRange   { location :: Location, tag :: a                                                               }
         | EmptyAST     { tag :: a                                                                                     }
    deriving (Eq,Show)


-- instance Show a => Show (AST a) where
--   show ast = drawAST 0 ast


space :: Char
space = ' '


putSpaces :: Int -> String
putSpaces   level = take level (repeat space)


putSpacesLn :: Int -> String
putSpacesLn level = "\n" `mappend` take level (repeat space)


putLocation :: Location -> String
putLocation location = " --- en el " `mappend` show location


putLocationLn :: Location -> String
putLocationLn location = " --- en el " `mappend` show location `mappend` "\n"


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



drawAST level ((DefProc name accs pre post bound ast)) = 
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
   


drawAST level ((Block action loc ast)) =
         putSpacesLn level `mappend` "Bloque: " `mappend` putLocation loc
                           `mappend` " //Tag: " `mappend` show ast 
                           `mappend` drawASTList (level + 4) action



drawAST level ((Skip loc ast)) =
         putSpacesLn level `mappend` "Saltar: " `mappend` putLocation loc 
                           `mappend` " //Tag: " `mappend` show ast 


drawAST level ((Abort loc ast)) =
         putSpacesLn level `mappend` "Abortar: " `mappend` putLocation loc 
                           `mappend` " //Tag: "  `mappend` show ast 


drawAST level ((Ran var loc ast)) =
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

     

drawAST level ((FCallExp loc name args ast)) =
         putSpacesLn level `mappend` "Llamada de la Función: " `mappend` show name `mappend` putLocation loc 
                           `mappend` " //Tag: "                `mappend` show ast 
         `mappend` putSpacesLn (level + 4) `mappend` "Argumentos: " `mappend` drawASTList (level + 8) args 



drawAST level ((ArrCall loc name args ast)) =
         putSpacesLn level `mappend` "Llamada del Arreglo: " `mappend` show name `mappend` putLocation loc 
                           `mappend` " //Tag: "              `mappend` show ast 
         `mappend` putSpacesLn (level + 4) `mappend` "Argumentos: " `mappend` drawASTList (level + 8) args 

                               

drawAST level ((ProcCall name args loc ast)) =
         putSpacesLn level `mappend` "Llamada del Procedimiento: " `mappend` show name `mappend` putLocation loc 
                           `mappend` " //Tag: "                    `mappend` show ast 
         `mappend` putSpacesLn (level + 4) `mappend` "Argumentos: " `mappend` drawASTList (level + 8) args 



drawAST level ((FunBody loc exp ast)) =
         putSpacesLn level `mappend` "Cuerpo de la Función: " `mappend` putLocation loc
                           `mappend` " //Tag: "               `mappend` show ast  
                           `mappend` drawAST(level + 4) (exp) 



drawAST level ((DefFun name _ body bound ast)) =
         putSpacesLn level `mappend` "Función: " `mappend` show name
                           `mappend` " //Tag: "   `mappend` show ast 
                           `mappend` drawAST(level + 4) bound   
                           `mappend` drawAST(level + 4) body   





drawAST _ (EmptyRange _ _) = "rango vacio"
drawAST _ (EmptyAST _) = "vacio"

--drawAST _ _ = show "No se creo el arbol"
--drawAST _ (ast) = show ast


drawASTList level xs = foldl (\acc d -> (acc `mappend` drawAST level (d))) [] xs


drawLAssign level idlist explist = foldl (\acc (id, exp) -> 
   (acc `mappend` putSpacesLn (level + 4) `mappend` "Variable: " `mappend` show ((fst . fst) id) 
        `mappend` putSpacesLn (level + 8) `mappend` "Lado derecho: "
        `mappend` drawAST (level + 12) (exp) )) [] $ zip idlist explist
