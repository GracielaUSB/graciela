module AST where

import qualified Data.Text as T
import Data.Monoid
import Location
import Token
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



data AST a = Arithmetic { opBinA   :: OpNum   , location :: Location, lexpr :: (AST a), rexp :: (AST a), argAST :: a      } -- ^ Operadores Matematicos de dos expresiones.
         | Boolean      { opBinB   :: OpBool  , location :: Location, lexpr :: (AST a), rexp :: (AST a), argAST :: a      } -- ^ Operadores Booleanos de dos expresiones.
         | Relational   { opBinR   :: OpRel   , location :: Location, lexpr :: (AST a), rexp :: (AST a), argAST :: a      } -- ^ Operadores Relacionales de dos expresiones.     
         | FCallExp     { location :: Location, fname    :: Token   , args     :: [AST a], argAST :: a                    } -- ^ Llamada a funcion.
         | ArrCall      { location :: Location, name     :: Token, list :: [AST a],        argAST :: a                    } -- ^ Búsqueda en arreglo.
         | ID           { location :: Location, id       :: Token, argAST :: a                                            } -- ^ Identificador.
         | Int          { location :: Location, exp      :: Token, argAST :: a                                            } -- ^ Numero entero.
         | Bool         { location :: Location, cbool    :: Token, argAST :: a                                            } -- ^ Tipo booleano con el token.
         | Char         { location :: Location, mchar    :: Token, argAST :: a                                            } -- ^ Tipo caracter con el token. 
         | String       { location :: Location, mstring  :: Token, argAST :: a                                            } -- ^ Tipo string con el token.
         | Constant     { location :: Location, int      :: Bool    , max    :: Bool,    argAST :: a                      } -- ^ Constantes.   
         | Convertion   { toType   :: Conv    , location :: Location, tiexp  :: (AST a), argAST :: a                      } -- ^ Conversión a entero.
         | Unary        { opUn     :: OpUn    , location :: Location, lenExp :: (AST a), argAST :: a                      } -- ^ Función raíz cuadrada.
         | LogicalNot   { location :: Location, loNotExp ::  (AST a), argAST :: a                                         } -- ^ Función raíz cuadrada.
         | Skip         { location :: Location, argAST :: a                                                               } -- ^ Instruccion Skip.
         | Abort        { location :: Location, argAST :: a                                                               } -- ^ Instruccion Abort.
         | Cond         { cguard   :: [AST a], location :: Location, argAST :: a                                          } -- ^ Instruccion If.
         | Rept         { rguard   :: [AST a], rinv   ::  (AST a),  rbound :: (AST a), location ::Location, argAST :: a   } -- ^ Instruccion Do.
         | Write        { ln       :: Bool,    wexp   ::  (AST a),  location :: Location, argAST :: a                     } -- ^ Escribir.
         | Block        { ldecla   :: [AST a], lisAct ::  [AST a],  location :: Location, argAST :: a                     }
         | FCall        { fname    :: Token,     args   :: [AST a], location :: Location, argAST :: a                     } -- ^ Llamada a funcion.
         | LAssign      { idlist   :: [(Token, [AST a])], explista :: [AST a], location :: Location, argAST :: a          } -- ^
         | Ran          { var      :: Token, location :: Location, argAST :: a                                            }
         | Guard        { gexp     :: (AST a), gact   ::  (AST a), location :: Location, argAST :: a                      } -- ^ Guardia.
         | GuardExp     { gexp     :: (AST a), gact   ::  (AST a), location :: Location, argAST :: a                      } -- ^ Guardia de Expresion.
         | DecVar       { lisidvar :: [Token], mytype ::  Type,    getAST  :: a                                           }
         | DecVarAgn    { lisidvar :: [Token], lexpdv :: [AST a],  mytype  :: Type,  argAST :: a                          }
         | DefFun       { fname    :: Token, fbody    ::  (AST a), lexprdf :: [AST a], nodeBound :: (AST a), argAST :: a  }
         | DefProc      { pname    :: Token, prbody   :: [AST a],  prargs  :: [AST a], nodePre :: (AST a) , nodePost  :: (AST a), nodeBound :: (AST a), argAST :: a }
         | DefProcDec   { pname    :: Token, prbody   :: [AST a],  prargs  :: [AST a], decs    :: (AST a) , nodePre   :: (AST a), nodePost  :: (AST a), nodeBound :: (AST a), argAST :: a }
         | Program      { pname    :: Token, listdef  :: [AST a],  listacc :: [AST a], argAST :: a                        }
         | FunBody      { fbexpr   :: (AST a), argAST :: a                                                                }
         | FunArg       { faid     :: Token, fatype :: Type, argAST :: a                                                  }
         | ArgType      { at       :: Token, argAST :: a                                                                  }
         | Arg          { argid    :: Token, atn :: TypeArg, atype :: Type, argAST :: a                                   }
         | DecProcReadFile { idfile  :: Token,   declist    :: [AST a], idlistproc ::[Token], argAST :: a                 }
         | DecProcReadSIO  { declist :: [AST a], idlistproc :: [Token], argAST :: a                                       }
         | DecProc         { declist :: [AST a], argAST :: a                                                              }
         | States          { tstate :: StateCond, exprlist      :: [AST a], argAST :: a                                   }
         | GuardAction     { assertionGa   :: (AST a), actionGa :: (AST a), argAST :: a                                   }
         | Quant           { opQ :: Token, varQ :: Token, rangeExp :: (AST a), termExpr :: (AST a), argAST :: a           }
         | EmptyAST
    deriving (Show, Eq)
