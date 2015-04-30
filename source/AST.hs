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

data AST a = Arithmetic  { getAST :: a, opBinA   :: OpNum   , location :: Location, lexpr :: (AST a), rexp :: (AST a) } -- ^ Operadores Matematicos de dos expresiones.
         | Boolean     { getAST :: a, opBinB   :: OpBool  , location :: Location, lexpr :: (AST a), rexp :: (AST a) } -- ^ Operadores Booleanos de dos expresiones.
         | Relational  { getAST :: a, opBinR   :: OpRel   , location :: Location, lexpr :: (AST a), rexp :: (AST a) } -- ^ Operadores Relacionales de dos expresiones.     
         | FCall       { getAST :: a, fname    :: Token   , args     :: [(AST a)]   , location :: Location      } -- ^ Llamada a funcion.
         | EqualNode   { getAST :: a, location :: Location, lexpr :: (AST a), rexp :: (AST a)                       }
         | FCallExp    { getAST :: a, fname    :: Token   , args     :: [(AST a)]   , location :: Location      } -- ^ Llamada a funcion.
         | ArrCall     { getAST :: a, location :: Location, name     :: Token   , list :: [(AST a)]             } -- ^ Búsqueda en arreglo.
         | Int         { getAST :: a, location :: Location, exp      :: Token                               } -- ^ Numero entero.
         | ID          { getAST :: a, location :: Location, id       :: Token                               } -- ^ Identificador.
         | Bool        { getAST :: a, location :: Location, cbool    :: Token                               } -- ^ Tipo booleano con el token.
         | Char        { getAST :: a, location :: Location, mchar    :: Token                               } -- ^ Tipo caracter con el token. 
         | String      { getAST :: a, location :: Location, mstring  :: Token                               } -- ^ Tipo string con el token.
         | Convertion  { getAST :: a, toType   :: Conv    , location :: Location, tiexp  :: (AST a)             } -- ^ Conversión a entero.
         | Unary       { getAST :: a, opUn     :: OpUn    , location :: Location, lenExp :: (AST a)             } -- ^ Función raíz cuadrada.
         | LogicalNot  { getAST :: a, location :: Location, loNotExp :: (AST a)                                 } -- ^ Función raíz cuadrada.
         | Constant    { getAST :: a, location :: Location, int      :: Bool    , max    :: Bool            } -- ^ Constantes.   
         | Skip        { getAST :: a, location :: Location                                                  } -- ^ Instruccion Skip.
         | Abort       { getAST :: a, location :: Location                                                  } -- ^ Instruccion Abort.
         | Cond        { getAST :: a, cguard   :: [(AST a)]   , location :: Location                            } -- ^ Instruccion If.
         | Rept        { getAST :: a, rguard   :: [(AST a)]   , rinv :: (AST a), rbound :: (AST a), location ::Location } -- ^ Instruccion Do.
         | Write       { getAST :: a, ln       :: Bool    , wexp :: (AST a), location :: Location               } -- ^ Escribir.
         | Guard       { getAST :: a, gexp     :: (AST a), gact :: (AST a), location :: Location                    } -- ^ Guardia.
         | GuardExp    { getAST :: a, gexp     :: (AST a), gact :: (AST a), location :: Location                    } -- ^ Guardia de Expresion.
         | LAssign     { getAST :: a, idlist   :: [(Token, [(AST a)])], explista :: [(AST a)], location :: Location } -- ^
         | Ran         { getAST :: a, var      :: Token, location :: Location                               }
         | Block       { getAST :: a, ldecla   :: [(AST a)], lisAct   :: [(AST a)], location :: Location            }
         | DecVar      { getAST :: a, lisidvar :: [Token], mytype ::  (AST a)                                   }
         | DecVarAgn   { getAST :: a, lisidvar :: [Token], lexpdv :: [(AST a)], mytype :: (AST a)                   }
         | BasicType   { getAST :: a, mybtype  :: Token                                                     }
         | ArrType     { getAST :: a, arrtype  :: (AST a)  , sizelist :: [(AST a)]                                  }
         | DefFun      { getAST :: a, fname    :: Token, ftype    ::  (AST a) , fbody   ::  (AST a) , lexprdf ::[(AST a)], nodeBound :: (AST a)                                     }
         | DefProc     { getAST :: a, pname    :: Token, prbody   :: [(AST a)], prargs  :: [(AST a)], nodePre :: (AST a) , nodePost  :: (AST a), nodeBound :: (AST a)                   }
         | DefProcDec  { getAST :: a, pname    :: Token, prbody   :: [(AST a)], prargs  :: [(AST a)], decs    :: (AST a) , nodePre   :: (AST a), nodePost  :: (AST a), nodeBound :: (AST a) }
         | Program     { getAST :: a, pname    :: Token, listdef  :: [(AST a)], listacc :: [(AST a)]                }
         | FunBody     { getAST :: a, fbexpr   :: (AST a)                                                       }
         | FunArg      { getAST :: a, faid     :: Token, fatype :: (AST a)                                      }
         | ArgType     { getAST :: a, at       :: Token                                                     }
         | Arg         { getAST :: a, argid    :: Token, atn :: (AST a), atype :: (AST a)                           }
         | DecProcReadFile { getAST :: a, idfile        :: Token, declist    :: [(AST a)], idlistproc ::[Token] }
         | DecProcReadSIO  { getAST :: a, declist       :: [(AST a)], idlistproc :: [Token]                     }
         | DecProc         { getAST :: a, declist       :: [(AST a)]                                            }
         | Precondition    { getAST :: a, preExp        :: [(AST a)]                                            }
         | Postcondition   { getAST :: a, postExp       :: [(AST a)]                                            }
         | Bound           { getAST :: a, boundExp      :: [(AST a)]                                            }
         | Assertion       { getAST :: a, assertionExp  :: [(AST a)]                                            }
         | Invariant       { getAST :: a, invarianteExp :: [(AST a)]                                            }
         | GuardAction     { getAST :: a, assertionGa   ::  (AST a), actionGa :: (AST a)                            }
         | Quant           { getAST :: a,  opQ :: Token, varQ :: Token, rangeExp :: (AST a), termExpr :: (AST a)     }
         | EmptyAST
    deriving (Show, Eq)
