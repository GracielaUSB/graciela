module AST where

import qualified Data.Text as T
import Token

{- |
   Tipo de dato que nos permite representar el árbol sintáctico abstracto
   del lenguaje. Los campos @line@ y @column@ representan la línea y columna, respectivamente,
   del nodo en el texto del programa.
 -}

data AST = SumNode                { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Suma de dos expresiones.
         | SubNode                { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Resta de dos expresiones.
         | MulNode                { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Multiplicación de dos expresiones.
         | DivNode                { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ División de dos expresiones.
         | ExpNode                { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Potencia de dos expresiones.
         | DisNode                { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Disjunción de dos expresiones.
         | ConNode                { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Conjunción de dos expresiones.
         | ModNode                { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Conjunción de dos expresiones.
         | EquNode                { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Equivalencia de dos expresiones.
         | EqualNode              { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Equivalencia de dos expresiones.
         | LessNode               { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Inequivalencia de dos expresiones.
         | GreaterNode            { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Inequivalencia de dos expresiones.
         | LEqualNode             { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Inequivalencia de dos expresiones.
         | GEqualNode             { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Inequivalencia de dos expresiones.
         | IneNode                { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Inequivalencia de dos expresiones.
         | ImpliesNode            { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Inequivalencia de dos expresiones.
         | ConseNode              { line' :: Int, column' :: Int, lexpr   :: AST  , rexp :: AST     } -- ^ Inequivalencia de dos expresiones.
         | FCallNode              { fname :: Token, args :: [AST], line' :: Int, column' :: Int     } -- ^ Llamada a funcion.
         | FCallExpNode           { fname :: Token, args :: [AST], line' :: Int, column' :: Int     } -- ^ Llamada a funcion.
         | ArrCallNode            { line' :: Int, column' :: Int, name    :: Token, list :: [AST]   } -- ^ Búsqueda en arreglo.
         | IntNode                { line' :: Int, column' :: Int, exp     :: Token                  } -- ^ Numero entero.
         | IDNode                 { line' :: Int, column' :: Int, id      :: Token                  } -- ^ Identificador.
         | BoolNode               { line' :: Int, column' :: Int, cbool   :: Token                  } -- ^ Tipo booleano con el token.
         | CharNode               { line' :: Int, column' :: Int, mchar   :: Token                  } -- ^ Tipo caracter con el token. 
         | StringNode             { line' :: Int, column' :: Int, mstring :: Token                  } -- ^ Tipo string con el token.
         | ProgNode               { line' :: Int, column' :: Int, prog    :: AST                    } -- ^ Raíz del árbol.
         | ToIntNode              { line' :: Int, column' :: Int, tiexp   :: AST                    } -- ^ Conversión a entero.
         | ToDoubleNode           { line' :: Int, column' :: Int, tdexp   :: AST                    } -- ^ Conversión a flotante.
         | ToStringNode           { line' :: Int, column' :: Int, tsexp   :: AST                    } -- ^ Conversión a string.
         | ToCharNode             { line' :: Int, column' :: Int, tcexp   :: AST                    } -- ^ Conversión a caracter.
         | MinusNode              { line' :: Int, column' :: Int, mexp    :: AST                    } -- ^ El negado de un número entero o flotante.
         | AbsNode                { line' :: Int, column' :: Int, absexp  :: AST                    } -- ^ Función valor absoluto.
         | SqrtNode               { line' :: Int, column' :: Int, sqrtexp :: AST                    } -- ^ Función raíz cuadrada.
         | LengthNode             { line' :: Int, column' :: Int, lengthexp :: AST                  } -- ^ Función raíz cuadrada.
         | LogicalNotNode         { line' :: Int, column' :: Int, lnotexp   :: AST                  } -- ^ Función raíz cuadrada.
         | MaxIntNode             { line' :: Int, column' :: Int                                    } -- ^ Constante entero máximo.
         | MinIntNode             { line' :: Int, column' :: Int                                    } -- ^ Constante entero mínimo.
         | MaxDouNode             { line' :: Int, column' :: Int                                    } -- ^ Constante flotante máximo.
         | MinDouNode             { line' :: Int, column' :: Int                                    } -- ^ Constante flotante mínimo.
         | SkipNode               { line' :: Int, column' :: Int                                    } -- ^ Constante flotante mínimo.
         | AbortNode              { line' :: Int, column' :: Int                                    } -- ^ Constante flotante mínimo.
         | CondNode               { cguard   :: [AST], line' :: Int, column' :: Int                   } -- ^ Constante flotante mínimo.
         | ReptNode               { rguard   :: [AST], rinv :: AST, rbound :: AST, line' :: Int, column' :: Int                   } -- ^ Constante flotante mínimo.
         | WriteNode              { wexp     :: AST, line' :: Int, column' :: Int                       } -- ^ Constante flotante mínimo.
         | WritelnNode            { wlnexp   :: AST, line' :: Int, column' :: Int                       } -- ^ Constante flotante mínimo.
         | GuardNode              { gexp     :: AST, gact :: AST, line' :: Int, column' :: Int          } -- ^ Constante flotante mínimo.
         | GuardExpNode              { gexp     :: AST, gact :: AST, line' :: Int, column' :: Int          } -- ^ Constante flotante mínimo.
         | LAssignNode            { idlist   :: [(Token, [AST])], explista :: [AST], line' :: Int, column' :: Int                 } -- ^ Constante flotante mínimo.
         | RanNode                { var      :: Token, line' :: Int, column' :: Int                      }
         | BlockNode              { ldecla   :: [AST], lisAct :: [AST], line' :: Int, column' :: Int  }
         | DecVarNode             { lisidvar :: [Token], mytype :: AST                              }
         | DecVarAgnNode          { lisidvar :: [Token], lexpdv :: [AST], mytype :: AST             }
         | BasicTypeNode          { mybtype  :: Token                                               }
         | ArrTypeNode            { arrtype  :: AST, sizelist :: [AST]                              }
         | DefFunNode             { fname    :: Token, ftype :: AST, fbody :: AST, lexprdf :: [AST], nodeBound :: AST }
         | DefProcNode            { pname    :: Token, prbody :: [AST], prargs :: [AST], nodePre :: AST, nodePost :: AST, nodeBound :: AST }
         | DefProcDecNode         { pname    :: Token, prbody :: [AST], prargs :: [AST], decs :: AST, nodePre :: AST, nodePost :: AST, nodeBound :: AST }
         | ProgramNode            { pname    :: Token, listdef :: [AST], listacc :: [AST] }
         | FunBodyNode            { fbexpr   :: AST }
         | FunArgNode             { faid :: Token, fatype :: AST    }
         | ArgTypeNode            { at :: Token }
         | ArgNode                { argid :: Token, atn :: AST, atype :: AST }
         | DecProcReadFileNode    { idfile :: Token, declist :: [AST], idlistproc :: [Token] }
         | DecProcReadSIONode     { declist :: [AST], idlistproc :: [Token] }
         | DecProcNode            { declist :: [AST] }
         | PreconditionNode       { preExp :: [AST] }
         | PostconditionNode      { postExp :: [AST] }
         | BoundNode              { boundExp :: [AST] }
         | AssertionNode          { assertionExp :: [AST] }
         | GuardActionNode        { assertionGa :: AST, actionGa :: AST }
         | InvariantNode          { invarianteExp :: [AST]          }
         | QuantNode              { opQ :: Token, varQ :: Token, rangeExp :: AST, termExpr :: AST }
         | EmptyAST
    deriving (Show)
