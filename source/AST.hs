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

data AST = Arithmetic  { getAST :: Int, opBinA   :: OpNum   , location :: Location, lexpr :: AST, rexp :: AST } -- ^ Operadores Matematicos de dos expresiones.
         | Boolean     { getAST :: Int, opBinB   :: OpBool  , location :: Location, lexpr :: AST, rexp :: AST } -- ^ Operadores Booleanos de dos expresiones.
         | Relational  { getAST :: Int, opBinR   :: OpRel   , location :: Location, lexpr :: AST, rexp :: AST } -- ^ Operadores Relacionales de dos expresiones.     
         | FCall       { getAST :: Int, fname    :: Token   , args     :: [AST]   , location :: Location      } -- ^ Llamada a funcion.
         | EqualNode   { getAST :: Int, location :: Location, lexpr :: AST, rexp :: AST                       }
         | FCallExp    { getAST :: Int, fname    :: Token   , args     :: [AST]   , location :: Location      } -- ^ Llamada a funcion.
         | ArrCall     { getAST :: Int, location :: Location, name     :: Token   , list :: [AST]             } -- ^ Búsqueda en arreglo.
         | Int         { getAST :: Int, location :: Location, exp      :: Token                               } -- ^ Numero entero.
         | ID          { getAST :: Int, location :: Location, id       :: Token                               } -- ^ Identificador.
         | Bool        { getAST :: Int, location :: Location, cbool    :: Token                               } -- ^ Tipo booleano con el token.
         | Char        { getAST :: Int, location :: Location, mchar    :: Token                               } -- ^ Tipo caracter con el token. 
         | String      { getAST :: Int, location :: Location, mstring  :: Token                               } -- ^ Tipo string con el token.
         | Convertion  { getAST :: Int, toType   :: Conv    , location :: Location, tiexp  :: AST             } -- ^ Conversión a entero.
         | Unary       { getAST :: Int, opUn     :: OpUn    , location :: Location, lenExp :: AST             } -- ^ Función raíz cuadrada.
         | LogicalNot  { getAST :: Int, location :: Location, loNotExp :: AST                                 } -- ^ Función raíz cuadrada.
         | Constant    { getAST :: Int, location :: Location, int      :: Bool    , max    :: Bool            } -- ^ Constantes.   
         | Skip        { getAST :: Int, location :: Location                                                  } -- ^ Instruccion Skip.
         | Abort       { getAST :: Int, location :: Location                                                  } -- ^ Instruccion Abort.
         | Cond        { getAST :: Int, cguard   :: [AST]   , location :: Location                            } -- ^ Instruccion If.
         | Rept        { getAST :: Int, rguard   :: [AST]   , rinv :: AST, rbound :: AST, location ::Location } -- ^ Instruccion Do.
         | Write       { getAST :: Int, ln       :: Bool    , wexp :: AST, location :: Location               } -- ^ Escribir.
         | Guard       { getAST :: Int, gexp     :: AST, gact :: AST, location :: Location                    } -- ^ Guardia.
         | GuardExp    { getAST :: Int, gexp     :: AST, gact :: AST, location :: Location                    } -- ^ Guardia de Expresion.
         | LAssign     { getAST :: Int, idlist   :: [(Token, [AST])], explista :: [AST], location :: Location } -- ^
         | Ran         { getAST :: Int, var      :: Token, location :: Location                               }
         | Block       { getAST :: Int, ldecla   :: [AST], lisAct   :: [AST], location :: Location            }
         | DecVar      { getAST :: Int, lisidvar :: [Token], mytype ::  AST                                   }
         | DecVarAgn   { getAST :: Int, lisidvar :: [Token], lexpdv :: [AST], mytype :: AST                   }
         | BasicType   { getAST :: Int, mybtype  :: Token                                                     }
         | ArrType     { getAST :: Int, arrtype  :: AST  , sizelist :: [AST]                                  }
         | DefFun      { getAST :: Int, fname    :: Token, ftype    ::  AST , fbody   ::  AST , lexprdf ::[AST], nodeBound :: AST                                     }
         | DefProc     { getAST :: Int, pname    :: Token, prbody   :: [AST], prargs  :: [AST], nodePre :: AST , nodePost  :: AST, nodeBound :: AST                   }
         | DefProcDec  { getAST :: Int, pname    :: Token, prbody   :: [AST], prargs  :: [AST], decs    :: AST , nodePre   :: AST, nodePost  :: AST, nodeBound :: AST }
         | Program     { getAST :: Int, pname    :: Token, listdef  :: [AST], listacc :: [AST]                }
         | FunBody     { getAST :: Int, fbexpr   :: AST                                                       }
         | FunArg      { getAST :: Int, faid     :: Token, fatype :: AST                                      }
         | ArgType     { getAST :: Int, at       :: Token                                                     }
         | Arg         { getAST :: Int, argid    :: Token, atn :: AST, atype :: AST                           }
         | DecProcReadFile { getAST :: Int, idfile        :: Token, declist    :: [AST], idlistproc ::[Token] }
         | DecProcReadSIO  { getAST :: Int, declist       :: [AST], idlistproc :: [Token]                     }
         | DecProc         { getAST :: Int, declist       :: [AST]                                            }
         | Precondition    { getAST :: Int, preExp        :: [AST]                                            }
         | Postcondition   { getAST :: Int, postExp       :: [AST]                                            }
         | Bound           { getAST :: Int, boundExp      :: [AST]                                            }
         | Assertion       { getAST :: Int, assertionExp  :: [AST]                                            }
         | Invariant       { getAST :: Int, invarianteExp :: [AST]                                            }
         | GuardAction     { getAST :: Int, assertionGa   ::  AST, actionGa :: AST                            }
         | Quant           { getAST :: Int,  opQ :: Token, varQ :: Token, rangeExp :: AST, termExpr :: AST     }
         | EmptyAST
    deriving (Show, Eq)
