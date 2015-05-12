module AST where

import qualified Data.Text as T
import Data.Monoid
import Location
import Token

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



data OpBool = Dis | Con  
      deriving (Eq)

instance Show OpBool where
   show Dis = "Disyunción"
   show Con = "Conjunción"



data OpRel = Equ | Less | Greater | LEqual | GEqual | Ine | Implies | Conse | Equal
      deriving (Eq) 

instance Show OpRel where
   show Equ     = "Equivalencia"
   show Less    = "Menor que"
   show Greater = "Mayor que"
   show LEqual  = "Menor o igual que"
   show GEqual  = "Mayor o igual que"
   show Ine     = "Negación"
   show Implies = "Implicación"
   show Conse   = "Consecuencia"
   show Equal   = "Inequivalencia"



data Conv = ToInt | ToDouble | ToString | ToChar  
      deriving (Eq) 

instance Show Conv where
   show ToInt    = "a Entero"
   show ToDouble = "a Flotante"
   show ToString = "a Cadenas de Caracteres"
   show ToChar   = "a Caracter"



data OpUn = Minus | Abs | Sqrt | Length  
      deriving (Eq) 

instance Show OpUn where
   show Minus  = "Negativo: "
   show Abs    = "Valor Absoluto: "
   show Sqrt   = "Raíz Cuadrada: "
   show Length = "Longitud: "


data StateCond = Pre | Post | Assertion | Bound | Invariant
      deriving (Eq)

instance Show StateCond where
   show Pre       = "Precondición: "
   show Post      = "Postcondición: "
   show Assertion = "Aserción: "
   show Bound     = "Función de Cota: "
   show Invariant = "Invariante: "



data AST a = Arithmetic { opBinA   :: OpNum   , location :: Location, lexpr :: (AST a), rexp :: (AST a), argAST :: (Maybe a)      } -- ^ Operadores Matematicos de dos expresiones.
         | Boolean      { opBinB   :: OpBool  , location :: Location, lexpr :: (AST a), rexp :: (AST a), argAST :: (Maybe a)      } -- ^ Operadores Booleanos de dos expresiones.
         | Relational   { opBinR   :: OpRel   , location :: Location, lexpr :: (AST a), rexp :: (AST a), argAST :: (Maybe a)      } -- ^ Operadores Relacionales de dos expresiones.     
         | FCallExp     { location :: Location, fname    :: Token   , args     :: [AST a], argAST :: (Maybe a)                    } -- ^ Llamada a funcion.
         | ArrCall      { location :: Location, name     :: Token, list :: [AST a],        argAST :: (Maybe a)                    } -- ^ Búsqueda en arreglo.
         | ID           { location :: Location, id       :: Token, argAST :: (Maybe a)                                            } -- ^ Identificador.
         | Int          { location :: Location, exp      :: Token, argAST :: (Maybe a)                                            } -- ^ Numero entero.
         | Bool         { location :: Location, cbool    :: Token, argAST :: (Maybe a)                                            } -- ^ Tipo booleano con el token.
         | Char         { location :: Location, mchar    :: Token, argAST :: (Maybe a)                                            } -- ^ Tipo caracter con el token. 
         | String       { location :: Location, mstring  :: Token, argAST :: (Maybe a)                                            } -- ^ Tipo string con el token.
         | Constant     { location :: Location, int      :: Bool    , max    :: Bool,    argAST :: (Maybe a)                      } -- ^ Constantes.   
         | Convertion   { toType   :: Conv    , location :: Location, tiexp  :: (AST a), argAST :: (Maybe a)                      } -- ^ Conversión a entero.
         | Unary        { opUn     :: OpUn    , location :: Location, lenExp :: (AST a), argAST :: (Maybe a)                      } -- ^ Función raíz cuadrada.
         | LogicalNot   { location :: Location, loNotExp ::  (AST a), argAST :: (Maybe a)                                         } -- ^ Función raíz cuadrada.
         | Skip         { location :: Location, argAST :: (Maybe a)                                                               } -- ^ Instruccion Skip.
         | Abort        { location :: Location, argAST :: (Maybe a)                                                               } -- ^ Instruccion Abort.
         | Cond         { cguard   :: [AST a], location :: Location, argAST :: (Maybe a)                                          } -- ^ Instruccion If.
         | Rept         { rguard   :: [AST a], rinv   ::  (AST a),  rbound :: (AST a), location ::Location, argAST :: (Maybe a)   } -- ^ Instruccion Do.
         | Write        { ln       :: Bool,    wexp   ::  (AST a),  location :: Location, argAST :: (Maybe a)                     } -- ^ Escribir.
         | Block        { lisAct ::  [AST a],  location :: Location, argAST :: (Maybe a)                     }
         | FCall        { fname    :: Token,     args   :: [AST a], location :: Location, argAST :: (Maybe a)                     } -- ^ Llamada a funcion.
         | LAssign      { idlist   :: [(Token, [AST a])], explista :: [AST a], location :: Location, argAST :: (Maybe a)          } -- ^
         | Ran          { var      :: Token, location :: Location, argAST :: (Maybe a)                                            }
         | Guard        { gexp     :: (AST a), gact   ::  (AST a), location :: Location, argAST :: (Maybe a)                      } -- ^ Guardia.
         | GuardExp     { gexp     :: (AST a), gact   ::  (AST a), location :: Location, argAST :: (Maybe a)                      } -- ^ Guardia de Expresion.
         | DefFun       { fname    :: Token, fbody     ::  (AST a), nodeBound :: (AST a), argAST :: (Maybe a) }
         | DefProc      { pname     :: Token, prbody    :: [AST a], nodePre   :: (AST a)
                         ,nodePost  :: (AST a), nodeBound :: (AST a), argAST    :: (Maybe a)                }
         | Program      { pname    :: Token, location  :: Location, listdef :: [AST a],  listacc :: [AST a], argAST :: (Maybe a)  }
         | FunBody      { location :: Location, fbexpr :: (AST a),  argAST :: (Maybe a)                                           }
         | States       { tstate   :: StateCond, location :: Location,  exprlist :: [AST a], argAST :: (Maybe a)                  }
         | GuardAction  { location :: Location, assertionGa :: (AST a), actionGa :: (AST a), argAST :: (Maybe a)                  }
         | Quant        { opQ :: Token, varQ :: Token, location :: Location, rangeExp :: (AST a), termExpr :: (AST a), argAST :: (Maybe a) }
         | EmptyAST
    deriving (Show, Eq)



space = ' '

putSpaces level = take level (repeat space)
putSpacesLn level = "\n" `mappend` take level (repeat space)

putLocation location = " --- en el " `mappend` show location
putLocationLn location = " --- en el " `mappend` show location `mappend` "\n"


verMaxMin True  True  = "MAX_INT"
verMaxMin True  False = "MIN_INT"
verMaxMin False True  = "MAX_DOUBLE"
verMaxMin False False = "MIN_DOUBLE"

verWrite True  = "Escribir con Salto de línea"
verWrite False = "Escribir"



drawAST level (Just (Program name loc defs accs ast)) = 
         putSpacesLn level `mappend` "Programa: " `mappend` show name `mappend` putLocation loc 
                           `mappend` drawASTList (level + 4) defs     `mappend` drawASTList (level + 4) accs



drawAST level (Just (DefProc name accs pre post bound ast)) = 
         putSpacesLn level `mappend` "Procedimiento: " `mappend` show name  -- `mappend` putLocation loc 
                           `mappend` putSpacesLn (level + 4) `mappend` drawAST (level + 4) (Just pre  )    
                           `mappend` putSpaces (level + 4) `mappend` drawAST (level + 8) (Just bound)  
                           `mappend` putSpacesLn (level + 4) `mappend` "Acciones: " `mappend` drawASTList (level + 8) accs
                           `mappend` putSpaces (level + 4) `mappend` drawAST (level + 4) (Just post )



drawAST level (Just (States t loc exprs ast)) = 
         putSpacesLn level `mappend` show t `mappend` putLocation loc       
                           `mappend` drawASTList (level + 4) exprs



drawAST level (Just (Arithmetic t loc lexpr rexp ast)) =
         putSpacesLn   level `mappend` "Operador Aritmético: " `mappend` show t `mappend` putLocation loc 
                           `mappend` putSpacesLn (level + 4) `mappend` "Lado izquierdo:" 
                                                             `mappend` drawAST (level + 8) (Just lexpr) 
                           `mappend` putSpacesLn (level + 4) `mappend` "Lado derecho:"  
                                                             `mappend` drawAST (level + 8) (Just rexp )      



drawAST level (Just (Relational t loc lexpr rexp ast)) =
         putSpacesLn level `mappend` "Operador Relacional: " `mappend` show t `mappend` putLocation loc 
                           `mappend` putSpacesLn (level + 4) `mappend` "Lado izquierdo:" 
                                                             `mappend` drawAST (level + 8) (Just lexpr) 
                           `mappend` putSpacesLn (level + 4) `mappend` "Lado derecho:"  
                                                             `mappend` drawAST (level + 8) (Just rexp )
                         


drawAST level (Just (Boolean t loc lexpr rexp ast)) =
         putSpacesLn level `mappend` "Operador Booleano: "   `mappend` show t `mappend` putLocation loc 
                           `mappend` putSpacesLn (level + 4) `mappend` "Lado izquierdo:" 
                                                             `mappend` drawAST (level + 8) (Just lexpr) 
                           `mappend` putSpacesLn (level + 4) `mappend` "Lado derecho:"  
                                                             `mappend` drawAST (level + 8) (Just rexp )
                        


drawAST level (Just (ID loc cont ast)) =
         putSpacesLn level `mappend` "ID: "        `mappend` show cont `mappend` putLocation loc 



drawAST level (Just (Int loc cont ast)) =
         putSpacesLn level `mappend` "Entero: "    `mappend` show cont `mappend` putLocation loc 



--drawAST level (Just (Float loc cont ast)) =
--         putSpacesLn level `mappend` "Flotante: "`mappend` show cont `mappend` putLocation loc 



drawAST level (Just (Bool loc cont ast)) =
         putSpacesLn level `mappend` "Booleano: "  `mappend` show cont `mappend` putLocation loc 



drawAST level (Just (Char loc cont ast)) =
         putSpacesLn level `mappend` "Caracter: "  `mappend` show cont `mappend` putLocation loc 



drawAST level (Just (String loc cont ast)) =
         putSpacesLn level `mappend` "String: "    `mappend` show cont `mappend` putLocation loc 



drawAST level (Just (Constant loc t max ast)) =
         putSpacesLn level `mappend` "Constante: " `mappend` verMaxMin t max `mappend` putLocation loc 



drawAST level (Just (LAssign idlist explist loc ast)) =
         putSpacesLn level `mappend` "Asignación: " `mappend` putLocation loc 
                           `mappend` drawLAssign (level) idlist explist



drawAST level (Just (Rept guard inv bound loc ast)) =
         putSpacesLn level `mappend` "Repetición: " `mappend` putLocation loc `mappend` drawASTList (level + 4) guard
                           `mappend` putSpaces (level + 4) `mappend` drawAST (level + 4) (Just inv   )
                           `mappend` putSpaces (level + 4) `mappend` drawAST (level + 4) (Just bound )



drawAST level (Just (Cond guard loc ast)) =
         putSpacesLn level `mappend` "Condicional: " `mappend` putLocation loc `mappend` drawASTList (level + 4) guard



drawAST level (Just (Guard exp action loc ast)) =
         putSpacesLn level `mappend` "Guardia: " `mappend` putLocation loc 
                           `mappend` drawAST (level + 4) (Just exp) 
                           `mappend` putSpacesLn (level + 4) `mappend` "Acciones:"  
                           `mappend` drawAST (level + 8) (Just action)
   


drawAST level (Just (GuardExp exp action loc ast)) =
         putSpacesLn level `mappend` "Guardia de Expresión: " `mappend` putLocation loc 
                           `mappend` drawAST (level + 4) (Just exp) 
                           `mappend` putSpacesLn (level + 4) `mappend` "Acciones:"  
                           `mappend` drawAST (level + 8) (Just action)
   


drawAST level (Just (Block action loc ast)) =
         putSpacesLn level `mappend` "Bloque: " `mappend` putLocation loc `mappend` drawASTList (level + 4) action



drawAST level (Just (Skip loc ast)) =
         putSpacesLn level `mappend` "Saltar: " `mappend` putLocation loc 



drawAST level (Just (Abort loc ast)) =
         putSpacesLn level `mappend` "Abortar: " `mappend` putLocation loc 



drawAST level (Just (Ran var loc ast)) =
         putSpacesLn level `mappend` "Aleatorio: " `mappend` show var `mappend` putLocation loc 

        

drawAST level (Just (Write ln exp loc ast)) =
         putSpacesLn level `mappend` verWrite ln `mappend` putLocation loc 
                           `mappend` drawAST (level + 4) (Just exp)



drawAST level (Just (GuardAction loc assert action  ast)) =
         putSpacesLn level `mappend` "Guardia de Acción: " `mappend` putLocation loc 
                           `mappend` drawAST (level + 4) (Just assert) 
                           `mappend` putSpacesLn (level + 4) `mappend` "Acciones:"  
                           `mappend` drawAST (level + 8) (Just action)
   


drawAST level (Just (Quant op var loc range term ast)) =
         putSpacesLn level `mappend` "Cuantificador: " `mappend` show op `mappend` putLocation loc 
         `mappend` putSpacesLn (level + 4) `mappend` "Variable cuantificada: " `mappend` show var          
         `mappend` putSpacesLn (level + 4) `mappend` "Rango: "  `mappend` drawAST (level + 8) (Just range) 
         `mappend` putSpacesLn (level + 4) `mappend` "Cuerpo: " `mappend` drawAST (level + 8) (Just term )  



drawAST level (Just (Convertion t loc exp ast)) =
         putSpacesLn level `mappend` "Conversión: " `mappend` show t `mappend` putLocation loc 
                           `mappend` drawAST (level + 4) (Just exp)  



drawAST level (Just (Unary op loc exp ast)) =
         putSpacesLn level `mappend` show op `mappend` putLocation loc 
                           `mappend` drawAST (level + 4) (Just exp)  


drawAST level (Just (LogicalNot loc exp ast)) =
         putSpacesLn level `mappend` "Negación: " `mappend` putLocation loc 
                           `mappend` drawAST (level + 4) (Just exp)  

                  


drawAST _ (Just EmptyAST) = "emp"
drawAST _ Nothing = show "No se creo el arbol"
drawAST _ (Just ast) = show ast



drawASTList level xs = foldl (\acc d -> (acc `mappend` drawAST level (Just d))) [] xs



drawLAssign level idlist explist = foldl (\acc (id, exp) -> 
   (acc `mappend` putSpacesLn (level + 4) `mappend` "Variable: " `mappend` show (fst id) 
        `mappend` putSpacesLn (level + 8) `mappend` "Lado derecho: "
        `mappend` drawAST (level + 12) (Just exp) )) [] $ zip idlist explist