module AST where

import qualified Data.Text as T
import Location
import Token

{- |
   Tipo de dato que nos permite representar el árbol sintáctico abstracto
   del lenguaje. Los campos @line@ y @column@ representan la línea y columna, respectivamente,
   del nodo en el texto del programa.
 -}

data OpNum = Sum | Sub | Mul | Div | Exp | Max | Min | Mod
      deriving (Show, Eq)

data OpBool = Dis | Con | Implies | Conse  
      deriving (Show, Eq)

data OpRel = Equ | Less | Greater | LEqual | GEqual | Ine 
      deriving (Show, Eq) 

data Conv = ToInt | ToDouble | ToString | ToChar  
      deriving (Show, Eq) 

data OpUn = Minus | Abs | Sqrt | Length  
      deriving (Show, Eq) 

data AST a = Arithmetic { opBinA   :: OpNum   , location :: Location, lexpr :: (AST a), rexp :: (AST a), getAST :: a      } -- ^ Operadores Matematicos de dos expresiones.
         | Boolean      { opBinB   :: OpBool  , location :: Location, lexpr :: (AST a), rexp :: (AST a), getAST :: a      } -- ^ Operadores Booleanos de dos expresiones.
         | Relational   { opBinR   :: OpRel   , location :: Location, lexpr :: (AST a), rexp :: (AST a), getAST :: a      } -- ^ Operadores Relacionales de dos expresiones.     
         | EqualNode    { location :: Location, lexpr    ::  (AST a), rexp  :: (AST a),      getAST :: a                  }
         | FCallExp     { fname    :: Token   , args     :: [(AST a)], location :: Location, getAST :: a                  } -- ^ Llamada a funcion.
         | ArrCall      { location :: Location, name     :: Token, list :: [(AST a)],        getAST :: a                  } -- ^ Búsqueda en arreglo.
         | ID           { location :: Location, id       :: Token, getAST :: a                                            } -- ^ Identificador.
         | Int          { getAST :: a, location :: Location, exp      :: Token                                            } -- ^ Numero entero.
         | Bool         { getAST :: a, location :: Location, cbool    :: Token                                            } -- ^ Tipo booleano con el token.
         | Char         { getAST :: a, location :: Location, mchar    :: Token                                            } -- ^ Tipo caracter con el token. 
         | String       { getAST :: a, location :: Location, mstring  :: Token                                            } -- ^ Tipo string con el token.
         | Constant     { location :: Location, int      :: Bool    , max    :: Bool,    getAST :: a                      } -- ^ Constantes.   
         | Convertion   { toType   :: Conv    , location :: Location, tiexp  :: (AST a), getAST :: a                      } -- ^ Conversión a entero.
         | Unary        { opUn     :: OpUn    , location :: Location, lenExp :: (AST a), getAST :: a                      } -- ^ Función raíz cuadrada.
         | LogicalNot   { location :: Location, loNotExp ::  (AST a), getAST :: a                                         } -- ^ Función raíz cuadrada.
         | Skip         { getAST :: a, location :: Location                                                             } -- ^ Instruccion Skip.
         | Abort        { getAST :: a, location :: Location                                                            } -- ^ Instruccion Abort.
         | Cond         { getAST :: a, cguard   :: [(AST a)], location :: Location                                      } -- ^ Instruccion If.
         | Rept         { getAST :: a, rguard   :: [(AST a)], rinv   ::  (AST a),  rbound :: (AST a), location ::Location } -- ^ Instruccion Do.
         | Write        { getAST :: a, ln       :: Bool,      wexp   ::  (AST a),  location :: Location                   } -- ^ Escribir.
         | Block        { getAST :: a, ldecla   :: [(AST a)], lisAct :: [(AST a)], location :: Location                   }
         | FCall        { getAST :: a, fname    :: Token,     args   :: [(AST a)], location :: Location                } -- ^ Llamada a funcion.
         | LAssign      { getAST :: a, idlist   :: [(Token, [(AST a)])], explista :: [(AST a)], location :: Location      } -- ^
         | Ran          { getAST :: a, var      :: Token, location :: Location                                        }
         | Guard        { gexp     :: (AST a), gact   ::  (AST a), location :: Location, getAST :: a                      } -- ^ Guardia.
         | GuardExp     { gexp     :: (AST a), gact   ::  (AST a), location :: Location, getAST :: a                      } -- ^ Guardia de Expresion.
         | DecVar       { lisidvar :: [Token], mytype ::  (AST a), getAST   :: a                                          }
         | DecVarAgn    { lisidvar :: [Token], lexpdv :: [(AST a)], mytype  :: (AST a),  getAST :: a                      }
         | BasicType    { mybtype  :: Token, getAST :: a                                                                  }
         | ArrType      { arrtype  :: (AST a), sizelist :: [(AST a)], getAST :: a                                         }
         | DefFun       { fname    :: Token, ftype    ::  (AST a) , fbody   ::  (AST a) , lexprdf ::[(AST a)], nodeBound :: (AST a), getAST :: a                                     }
         | DefProc      { pname    :: Token, prbody   :: [(AST a)], prargs  :: [(AST a)], nodePre :: (AST a) , nodePost  :: (AST a), nodeBound :: (AST a), getAST :: a         }
         | DefProcDec   { pname    :: Token, prbody   :: [(AST a)], prargs  :: [(AST a)], decs    :: (AST a) , nodePre   :: (AST a), nodePost  :: (AST a), nodeBound :: (AST a), getAST :: a}
         | Program      { pname    :: Token, listdef  :: [(AST a)], listacc :: [(AST a)], getAST :: a            }
         | FunBody      { fbexpr   :: (AST a), getAST :: a                                                       }
         | FunArg       { faid     :: Token, fatype :: (AST a), getAST :: a                                      }
         | ArgType      { at       :: Token, getAST :: a                                                         }
         | Arg          { argid    :: Token, atn :: (AST a), atype :: (AST a), getAST :: a                       }
         | DecProcReadFile { idfile        :: Token, declist :: [(AST a)], idlistproc ::[Token], getAST :: a     }
         | DecProcReadSIO  { declist       :: [(AST a)], idlistproc :: [Token], getAST :: a                      }
         | DecProc         { declist       :: [(AST a)], getAST :: a                                             }
         | Precondition    { preExp        :: [(AST a)], getAST :: a                                             }
         | Postcondition   { postExp       :: [(AST a)], getAST :: a                                             }
         | Bound           { boundExp      :: [(AST a)], getAST :: a                                             }
         | Assertion       { assertionExp  :: [(AST a)], getAST :: a                                             }
         | Invariant       { invarianteExp :: [(AST a)], getAST :: a                                             }
         | GuardAction     { assertionGa   :: (AST a), actionGa :: (AST a), getAST :: a                          }
         | Quant           { opQ :: Token, varQ :: Token, rangeExp :: (AST a), termExpr :: (AST a), getAST :: a  }
         | EmptyAST
    deriving (Show, Eq)
