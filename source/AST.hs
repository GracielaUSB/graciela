module AST where

import qualified Data.Text as T
import Location
import Token
import Data.Monoid
import Type

{- |
   Tipo de dato que nos permite representar el árbol sintáctico abstracto
   del lenguaje. Los campos @line@ y @column@ representan la línea y columna, respectivamente,
   del nodo en el texto del programa.
 -}

data OpNum = Sum | Sub | Mul | Div | Exp | Max | Min | Mod
      deriving (Show, Eq)

data OpBool = Dis | Con  
      deriving (Show, Eq)

data OpRel = Equ | Less | Greater | LEqual | GEqual | Ine | Implies | Conse | Equal
      deriving (Show, Eq) 

data Conv = ToInt | ToDouble | ToString | ToChar  
      deriving (Show, Eq) 

data OpUn = Minus | Abs | Sqrt | Length  
      deriving (Show, Eq) 

data TypeArg = In | Out | InOut
      deriving (Show, Eq)

data StateCond = Pre | Post | Assertion | Bound | Invariant
      deriving (Show, Eq)


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
         | Block        { ldecla   :: [AST a], lisAct ::  [AST a],  location :: Location, argAST :: (Maybe a)                     }
         | FCall        { fname    :: Token,     args   :: [AST a], location :: Location, argAST :: (Maybe a)                     } -- ^ Llamada a funcion.
         | LAssign      { idlist   :: [(Token, [AST a])], explista :: [AST a], location :: Location, argAST :: (Maybe a)          } -- ^
         | Ran          { var      :: Token, location :: Location, argAST :: (Maybe a)                                            }
         | Guard        { gexp     :: (AST a), gact   ::  (AST a), location :: Location, argAST :: (Maybe a)                      } -- ^ Guardia.
         | GuardExp     { gexp     :: (AST a), gact   ::  (AST a), location :: Location, argAST :: (Maybe a)                      } -- ^ Guardia de Expresion.
         | DecProcReadFile { idfile  :: Token,   declist    :: [AST a], idlistproc ::[Token], argAST :: (Maybe a)                 }
         | DecProcReadSIO  { declist :: [AST a], idlistproc :: [Token], argAST :: (Maybe a)                                       }
         | DecProc         { declist :: [AST a], argAST :: (Maybe a)                                                              }
         | DecVar       { lisidvar :: [Token], mytype  ::  Type,    getAST  :: (Maybe a)                                          }
         | DecVarAgn    { lisidvar :: [Token], lexpdv  :: [AST a],  mytype  :: Type,  argAST :: (Maybe a)                         }
         | DefFun       { fname    :: Token, fbody     ::  (AST a), lexprdf :: [AST a], nodeBound :: (AST a), argAST :: (Maybe a) }
         | DefProc      { pname    :: Token, prbody    :: [AST a],  prargs  :: [AST a], nodePre :: (AST a) , nodePost  :: (AST a), nodeBound :: (AST a), argAST :: (Maybe a) }
         | DefProcDec   { pname    :: Token, prbody    :: [AST a],  prargs  :: [AST a], decs    :: (AST a) , nodePre   :: (AST a), nodePost  :: (AST a), nodeBound :: (AST a), argAST :: (Maybe a) }
         | Program      { pname    :: Token, location  :: Location, listdef :: [AST a],  listacc :: [AST a], argAST :: (Maybe a)  }
         | FunBody      { location :: Location, fbexpr :: (AST a),  argAST :: (Maybe a)                                           }
         | FunArg       { faid     :: Token, location  :: Location, fatype :: Type, argAST :: (Maybe a)                           }
         | Arg          { argid    :: Token, location  :: Location, atn :: TypeArg, atype :: Type, argAST :: (Maybe a)            }
         | States       { tstate   :: StateCond, location :: Location,  exprlist :: [AST a], argAST :: (Maybe a)                  }
         | GuardAction  { location :: Location, assertionGa :: (AST a), actionGa :: (AST a), argAST :: (Maybe a)                  }
         | Quant        { opQ :: Token, varQ :: Token, location :: Location, rangeExp :: (AST a), termExpr :: (AST a), argAST :: (Maybe a) }
         | EmptyAST
    deriving (Show, Eq)

space = ' '

putSpaces level = take level (repeat space)

drawAST level (Just (Program name location defs accs ast)) = putSpaces level `mappend` "Programa : " `mappend` show name `mappend` "\n" 
                                         `mappend` drawASTList (level + 1) defs 
                                         `mappend` drawASTList (level + 1) accs

drawAST level (Just (DefProcDec name accs args decs pre post bound ast)) = putSpaces level `mappend` "Procedimiento: " `mappend` show name `mappend` "\n"   
                                         `mappend` putSpaces level `mappend` "Argumentos:\n"     `mappend` drawASTList (level + 1) args
                                         `mappend` putSpaces level `mappend` "Precondicion:\n"   `mappend` drawAST (level + 1) (Just pre  )    
                                         `mappend` putSpaces level `mappend` "Funcion de cota: " `mappend` drawAST (level + 1) (Just bound)  
                                         `mappend` putSpaces level `mappend` "Acciones: "        `mappend` drawASTList (level + 1) accs
                                         `mappend` putSpaces level `mappend` "Postcondicion: "   `mappend` drawAST (level + 1) (Just post )

drawAST level (Just (Arg name location carg targ ast)) = putSpaces level `mappend` show name `mappend` " Comportamiento: " `mappend` show carg `mappend` " Tipo: " `mappend` show targ  

drawAST level (Just (States t location exprs ast))     = putSpaces level `mappend` show t `mappend` drawASTList (level + 1) exprs



drawAST _ Nothing = show "No se creo el arbol"
drawAST _ ast = show (Just ast)

--drawAST _ _       = show "yeii" 

drawASTList level xs = foldl (\acc d -> (acc `mappend` drawAST level (Just d)) `mappend` "\n") [] xs

