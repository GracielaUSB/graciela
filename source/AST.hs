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

data AST a = Arithmetic { opBinA   :: OpNum   , location :: Location, lexpr :: (AST a), rexp :: (AST a), argAST :: (Maybe a)      } -- ^ Operadores Matematicos de dos expresiones.
         | Boolean      { opBinB   :: OpBool  , location :: Location, lexpr :: (AST a), rexp :: (AST a), argAST :: (Maybe a)      } -- ^ Operadores Booleanos de dos expresiones.
         | Relational   { opBinR   :: OpRel   , location :: Location, lexpr :: (AST a), rexp :: (AST a), argAST :: (Maybe a)      } -- ^ Operadores Relacionales de dos expresiones.     
         | EqualNode    { location :: Location, lexpr    ::  (AST a), rexp  :: (AST a),      argAST :: (Maybe a)                  }
         | FCallExp     { fname    :: Token   , args     :: [(AST a)], location :: Location, argAST :: (Maybe a)                  } -- ^ Llamada a funcion.
         | ArrCall      { location :: Location, name     :: Token, list :: [(AST a)],        argAST :: (Maybe a)                  } -- ^ Búsqueda en arreglo.
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
         | Cond         { cguard   :: [(AST a)], location :: Location, argAST :: (Maybe a)                                        } -- ^ Instruccion If.
         | Rept         { rguard   :: [(AST a)], rinv   ::  (AST a),  rbound :: (AST a), location ::Location, argAST :: (Maybe a) } -- ^ Instruccion Do.
         | Write        { ln       :: Bool,      wexp   ::  (AST a),  location :: Location, argAST :: (Maybe a)                   } -- ^ Escribir.
         | Block        { ldecla   :: [(AST a)], lisAct :: [(AST a)], location :: Location, argAST :: (Maybe a)                   }
         | FCall        { fname    :: Token,     args   :: [(AST a)], location :: Location, argAST :: (Maybe a)                   } -- ^ Llamada a funcion.
         | LAssign      { idlist   :: [(Token, [(AST a)])], explista :: [(AST a)], location :: Location, argAST :: (Maybe a)      } -- ^
         | Ran          { var      :: Token, location :: Location, argAST :: (Maybe a)                                            }
         | Guard        { gexp     :: (AST a), gact   ::  (AST a), location :: Location, argAST :: (Maybe a)                      } -- ^ Guardia.
         | GuardExp     { gexp     :: (AST a), gact   ::  (AST a), location :: Location, argAST :: (Maybe a)                      } -- ^ Guardia de Expresion.
         | DecVar       { lisidvar :: [Token], mytype ::  (AST a), getAST   :: a                                          }
         | DecVarAgn    { lisidvar :: [Token], lexpdv :: [(AST a)], mytype  :: (AST a),  argAST :: (Maybe a)                      }
         | BasicType    { mybtype  :: Token, argAST :: (Maybe a)                                                                  }
         | ArrType      { arrtype  :: (AST a), sizelist :: [(AST a)], argAST :: (Maybe a)                                         }
         | DefFun       { fname    :: Token, ftype    ::  (AST a) , fbody   ::  (AST a) , lexprdf ::[(AST a)], nodeBound :: (AST a), argAST :: (Maybe a)                                     }
         | DefProc      { pname    :: Token, prbody   :: [(AST a)], prargs  :: [(AST a)], nodePre :: (AST a) , nodePost  :: (AST a), nodeBound :: (AST a), argAST :: (Maybe a)         }
         | DefProcDec   { pname    :: Token, prbody   :: [(AST a)], prargs  :: [(AST a)], decs    :: (AST a) , nodePre   :: (AST a), nodePost  :: (AST a), nodeBound :: (AST a), argAST :: (Maybe a)}
         | Program      { pname    :: Token, listdef  :: [(AST a)], listacc :: [(AST a)], argAST :: (Maybe a)            }
         | FunBody      { fbexpr   :: (AST a), argAST :: (Maybe a)                                                       }
         | FunArg       { faid     :: Token, fatype :: (AST a), argAST :: (Maybe a)                                      }
         | ArgType      { at       :: Token, argAST :: (Maybe a)                                                         }
         | Arg          { argid    :: Token, atn :: (AST a), atype :: (AST a), argAST :: (Maybe a)                       }
         | DecProcReadFile { idfile        :: Token, declist :: [(AST a)], idlistproc ::[Token], argAST :: (Maybe a)     }
         | DecProcReadSIO  { declist       :: [(AST a)], idlistproc :: [Token], argAST :: (Maybe a)                      }
         | DecProc         { declist       :: [(AST a)], argAST :: (Maybe a)                                             }
         | Precondition    { preExp        :: [(AST a)], argAST :: (Maybe a)                                             }
         | Postcondition   { postExp       :: [(AST a)], argAST :: (Maybe a)                                             }
         | Bound           { boundExp      :: [(AST a)], argAST :: (Maybe a)                                             }
         | Assertion       { assertionExp  :: [(AST a)], argAST :: (Maybe a)                                             }
         | Invariant       { invarianteExp :: [(AST a)], argAST :: (Maybe a)                                             }
         | GuardAction     { assertionGa   :: (AST a), actionGa :: (AST a), argAST :: (Maybe a)                          }
         | Quant           { opQ :: Token, varQ :: Token, rangeExp :: (AST a), termExpr :: (AST a), argAST :: (Maybe a)  }
         | EmptyAST
    deriving (Show, Eq)
