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

data AST = Arithmetic  { opBinA   :: OpNum   , location :: Location, lexpr :: AST, rexp :: AST } -- ^ Operadores Matematicos de dos expresiones.
         | Boolean     { opBinB   :: OpBool  , location :: Location, lexpr :: AST, rexp :: AST } -- ^ Operadores Booleanos de dos expresiones.
         | Relational  { opBinR   :: OpRel   , location :: Location, lexpr :: AST, rexp :: AST } -- ^ Operadores Relacionales de dos expresiones.     
         | FCall       { fname    :: Token   , args     :: [AST]   , location :: Location      } -- ^ Llamada a funcion.
         | EqualNode   { location :: Location, lexpr :: AST, rexp :: AST                       }
         | FCallExp    { fname    :: Token   , args     :: [AST]   , location :: Location      } -- ^ Llamada a funcion.
         | ArrCall     { location :: Location, name     :: Token   , list :: [AST]             } -- ^ Búsqueda en arreglo.
         | Int         { location :: Location, exp      :: Token                               } -- ^ Numero entero.
         | ID          { location :: Location, id       :: Token                               } -- ^ Identificador.
         | Bool        { location :: Location, cbool    :: Token                               } -- ^ Tipo booleano con el token.
         | Char        { location :: Location, mchar    :: Token                               } -- ^ Tipo caracter con el token. 
         | String      { location :: Location, mstring  :: Token                               } -- ^ Tipo string con el token.
         | Prog        { location :: Location, prog     :: AST                                 } -- ^ Raíz del árbol.
         | Convertion  { toType   :: Conv    , location :: Location, tiexp  :: AST             } -- ^ Conversión a entero.
         | Unary       { opUn     :: OpUn    , location :: Location, lenExp :: AST             } -- ^ Función raíz cuadrada.
         | LogicalNot  { location :: Location, loNotExp :: AST                                 } -- ^ Función raíz cuadrada.
         | Constant    { location :: Location, int      :: Bool    , max    :: Bool            } -- ^ Constantes.   
         | Skip        { location :: Location                                                  } -- ^ Instruccion Skip.
         | Abort       { location :: Location                                                  } -- ^ Instruccion Abort.
         | Cond        { cguard   :: [AST]   , location :: Location                            } -- ^ Instruccion If.
         | Rept        { rguard   :: [AST]   , rinv :: AST, rbound :: AST, location ::Location } -- ^ Instruccion Do.
         | Write       { ln       :: Bool    , wexp :: AST, location :: Location               } -- ^ Escribir.
         | Guard       { gexp     :: AST, gact :: AST, location :: Location                    } -- ^ Guardia.
         | GuardExp    { gexp     :: AST, gact :: AST, location :: Location                    } -- ^ Guardia de Expresion.
         | LAssign     { idlist   :: [(Token, [AST])], explista :: [AST], location :: Location } -- ^
         | Ran         { var      :: Token, location :: Location                               }
         | Block       { ldecla   :: [AST], lisAct   :: [AST], location :: Location            }
         | DecVar      { lisidvar :: [Token], mytype ::  AST                                   }
         | DecVarAgn   { lisidvar :: [Token], lexpdv :: [AST], mytype :: AST                   }
         | BasicType   { mybtype  :: Token                                                     }
         | ArrType     { arrtype  :: AST  , sizelist :: [AST]                                  }
         | DefFun      { fname    :: Token, ftype    ::  AST , fbody   ::  AST , lexprdf ::[AST], nodeBound :: AST                                     }
         | DefProc     { pname    :: Token, prbody   :: [AST], prargs  :: [AST], nodePre :: AST , nodePost  :: AST, nodeBound :: AST                   }
         | DefProcDec  { pname    :: Token, prbody   :: [AST], prargs  :: [AST], decs    :: AST , nodePre   :: AST, nodePost  :: AST, nodeBound :: AST }
         | Program     { pname    :: Token, listdef  :: [AST], listacc :: [AST]                }
         | FunBody     { fbexpr   :: AST                                                       }
         | FunArg      { faid     :: Token, fatype :: AST                                      }
         | ArgType     { at       :: Token                                                     }
         | Arg         { argid    :: Token, atn :: AST, atype :: AST                           }
         | DecProcReadFile { idfile        :: Token, declist    :: [AST], idlistproc ::[Token] }
         | DecProcReadSIO  { declist       :: [AST], idlistproc :: [Token]                     }
         | DecProc         { declist       :: [AST]                                            }
         | Precondition    { preExp        :: [AST]                                            }
         | Postcondition   { postExp       :: [AST]                                            }
         | Bound           { boundExp      :: [AST]                                            }
         | Assertion       { assertionExp  :: [AST]                                            }
         | GuardAction     { assertionGa   ::  AST, actionGa :: AST                            }
         | Invariant       { invarianteExp :: [AST]                                            }
         | Quant           { opQ :: Token, varQ :: Token, rangeExp :: AST, termExpr :: AST     }
         | EmptyAST
    deriving (Show, Eq)