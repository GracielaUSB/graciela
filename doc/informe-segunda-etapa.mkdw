# Informe segunda etapa

## Procedo de parseo

Continuando con el trabajo del trimestre pasado, apmliamos el parser de la calculadora de ejemplo para hacer el parser del lenguaje. El parser es del tipo recursivo descendente, aprovechando el hecho que la gramática es **LL(1)**, usando la librería *parsec* del lenguaje *haskell*. Esta librería nos permite de forma sencilla trasladar la gramática del lenguaje a un reconocedor, ya que está basada en el uso de combinadores de reconocedores para su funcionamiento, lo que nos permite crear reconocedores para construcciones del lenguaje a partir de otros más simples.

Además, la librería nos permite diseñar un parser con recuperación de errores de manera muy sencilla, ya que el programador tiene control sobre la forma en que se realiza el proceso de reconocimiento.  Por lo que podemos diseñar el reconocedor de manera que al momento de extraer de la entrada un símbolo inesperado pueda seguir leyendo símbolos hasta recuperarse en algún otro desde el cual pueda seguir el reconocimiento. A estos errores los llamaremos **errores de sintaxis**

Si el proceso de reconocimiento termina, el parser retorna la representación intermedia del programa en forma de árbol. Éste contiene toda la información necesaria para continuar con el proceso de compilación. Para la representación del árbol usamos un tipo algebraico polimórfico provisto por *haskell*. Para esta etapa era necesario crear un árbol que contuviera información parcial de los tipos del lenguaje encontrados durante el reconocimiento, pero para etapas siguientes necesitaremos que éste contenga el código intermedio para ser reconocido por la herramienta que generarará el código final, **LLVM** en nuestro caso. Esta es una de las ventajas de usar un lenguaje como *haskell* que ofrece tipos polimórficos, podemos usar la misma representación para el árbol y almacenar distintas estructuras de datos, dependiendo del momento de la compilación en la que nos encontremos, solo debemos preocuparnos por las transformaciones necesarias.

La representación usada es la siguiente:

    data AST a = 
        Arithmetic  { opBinA   :: OpNum
                    , location :: Location
                    , lexpr    :: AST a
                    , rexp     :: AST a
                    , tag      :: a      
                    } -- ^ Operadores Matematicos de dos expresiones.
      | Boolean     { opBinB   :: OpBool
                    , location :: Location
                    , lexpr    :: AST a
                    , rexp     :: AST a
                    , tag :: a
                    } -- ^ Operadores Booleanos de dos expresiones.
      | Relational  { opBinR   :: OpRel
                    , location :: Location
                    , lexpr    :: AST a
                    , rexp     :: AST a
                    , tag      :: a
                    } -- ^ Operadores Relacionales de dos expresiones.     
      | ArrCall     { location :: Location
                    , name     :: T.Text
                    , list :: [AST a]
                    , tag :: a
                    } -- ^ Búsqueda en arreglo.
      | ID          { location :: Location
                    , id       :: T.Text
                    , tag      :: a
                    } -- ^ Identificador.
      | Int         { location :: Location
                    , expInt   :: Integer
                    , tag :: a
                    } -- ^ Numero entero.
      | Float       { location :: Location
                    , expFloat :: Double
                    , tag :: a
                    } -- ^ Numero entero.
      | Bool        { location :: Location
                    , cbool    :: Bool
                    , tag      :: a
                    } -- ^ Tipo booleano con el token.
      | Char        { location :: Location
                    , mchar    :: Char
                    , tag      :: a
                    } -- ^ Tipo caracter con el token. 
      | String      { location :: Location
                    , mstring  :: String
                    , tag :: a
                    } -- ^ Tipo string con el token.
      | Constant    { location :: Location
                    , int      :: Bool
                    , max      :: Bool
                    , tag      :: a 
                    } -- ^ Constantes.   
      | Convertion  { toType   :: Conv
                    , location :: Location
                    , tiexp    :: AST a
                    , tag :: a
                    } -- ^ Conversión a entero.
      | Unary       { opUn     :: OpUn
                    , location :: Location
                    , lenExp   :: AST a
                    , tag      :: a
                    } -- ^ Función raíz cuadrada.
      | Skip        { location :: Location
                    , tag      :: a
                    } -- ^ Instruccion Skip.
      | Abort       { location :: Location
                    , tag      :: a
                    } -- ^ Instruccion Abort.
      | Cond        { cguard   :: [AST a]
                    , location :: Location
                    , tag      :: a
                    } -- ^ Instruccion If.
      | Block       { location :: Location
                    , listDec  :: [AST a]
                    , lisAct   :: [AST a]
                    , tag      :: a
                    }
      | Rept        { rguard   :: [AST a]
                    , rinv     :: AST a
                    , rbound   :: AST a
                    , location :: Location
                    , tag      :: a
                    } -- ^ Instruccion Do.
      | LAssign     { idlist   :: [((T.Text, Type), [AST a])]
                    , explista :: [AST a]
                    , location :: Location
                    , tag      :: a
                    } 
      | Write       { ln       :: Bool
                    , wexp     :: (AST a)
                    , location :: Location
                    , tag      :: a
                    } -- ^ Escribir.
      | FCallExp    { location :: Location
                    , fname    :: T.Text
                    , args     :: [AST a]
                    , tag :: a
                    } -- ^ Llamada a funcion.
      | ProcCall    { pname     :: T.Text
                    , astSTable :: SymbolTable
                    , location  :: Location
                    , args      :: [AST a]
                    , tag :: a                 
                    } 
      | ConsAssign  { location :: Location
                    , caID     :: [(T.Text, Location)]
                    , caExpr   :: [AST a]
                    , tag      :: a 
                    }
      | Guard       { gexp     :: AST a
                    , gact     :: AST a
                    , location :: Location
                    , tag      :: a
                    } -- ^ Guardia.
      | GuardExp    { gexp     :: AST a
                    , gact     :: AST a
                    , location :: Location
                    , tag      :: a
                    } -- ^ Guardia de Expresion.
      | DefFun      { dfname    :: T.Text
                    , location  :: Location
                    , fbody     :: AST a
                    , nodeBound :: AST a
                    , tag       :: a
                    }
      | DefProc     { pname     :: T.Text
                    , prbody    :: [AST a]
                    , nodePre   :: (AST a)
                    , nodePost  :: (AST a)
                    , nodeBound :: (AST a)
                    , constDec  :: [AST a]
                    , tag       :: a
                    }
      | Ran         { var      :: T.Text
                    , location :: Location
                    , tag      :: a
                    }
      | Program     { pname    :: T.Text
                    , location :: Location
                    , listdef  :: [AST a]
                    , listacc  :: [AST a]
                    , tag      :: a
                    }
      | FunBody     { location :: Location
                    , fbexpr   :: AST a
                    , tag      :: a
                    }
      | GuardAction { location    :: Location
                    , assertionGa :: AST a
                    , actionGa    :: AST a
                    , tag         :: a 
                    }
      | States      { tstate   :: StateCond
                    , location :: Location
                    , exps     :: AST a
                    , tag      :: a
                    }
      | Quant       { opQ      :: OpQuant
                    , varQ     :: T.Text
                    , location :: Location
                    , rangeExp :: AST a
                    ,termExpr  :: AST a
                    , tag      :: a
                    }
      | QuantRan    { opQ       :: OpQuant
                    , varQ      :: T.Text
                    , location  :: Location
                    , rangeVExp :: [RA.Range Integer]
                    , termExpr  :: AST a
                    , tag       :: a
                    }
      | EmptyRange  { location :: Location
                    , tag      :: a
                    }
      | EmptyAST    { tag :: a }
        deriving (Eq)

El árbol retornado por el parser tiene información parcial de los tipos 
encontrados en el programa. Aprovechando el hecho que en el lenguaje todo símbolo debe ser definido antes de ser usado, en cualquier momento del reconocimiento la tabla de símbolos contiene información de los símbolos alcanzables en la parte del programa que está siendo reconocida, por lo que el parser cuando encuentra una variable puede consultar la tabla de símbolos y de una vez asociar su tipo. En caso de no ser encontrada en la tabla, hemos encontrado un error el cual es acumulado y el reconocedor continúa. A estos errores los llamaremos **errores de contexto**.

Vale la pena recalcar que esta técnica no puede ser usada con las llamadas funciones y procedimientos porque no se exigue un órden en la definición de estos, una función o procedimiento puede ser definido **después** de ser llamado. En estos casos, la verificación es dejada a una etapa posterior de compilación.

## Tabla de símbolos

Definición: 

     newtype Diccionario = 
         Diccionario { getMap :: M.Map T.Text (Contents SymbolTable) }
             deriving (Eq)
    
     data SymbolTable = 
         Table { actual :: Tr.Tree ((Diccionario, Scope), Maybe SymbolTable) }  
             deriving (Eq)

Para mantener información de las variables, funciones y procedimientos reconocidos usamos una **tabla de símbolos**. Para modelarla usamos un árbol de diccionarios. Cada nodo del árbol es un diccionario el cual contiene información de cada uno de los símbolos que fueron definidos en el alcance que representa. La raíz del árbol contiene información de las funciones y procedimientos definidos en el programa mientras que cada una de las ramas almacena información de los símbolos definidos en cada uno de los alcances. Para que un nodo sea rama de otro, el alcance que representa debe estar definido dentro del alcance que representa el nodo padre. 

Además, cada nodo mantiene un apuntador a su nodo padre para hacer posible la búsqueda de cada símbolo, por lo que al encontrar un símbolo se busca en el nodo actual y en caso de no ser encontrado se busca en el nodo padre hasta llegar a la raíz en caso de no ser encontrado en los nodos intermedios. Para representar el árbol se usó la librería *Data.Tree* y para los diccionarios *Data.Map*.

### Estado

Para poder usar la tabla de símbolos es necesario hacer uso del **monad estado**. Este nos permite consultar y modificar la tabla de símbolos en cualquier momento mientras se realiza el proceso de parseo. Además, también llevamos dos listas, una para los errores de sintaxis y otra para los errores de contexto. Por lo que cada vez que encontramos un error simplemente lo agregamos a la lista correspondiente y podemos continuar con el proceso de reconocimiento.

    data ParserState = 
            ParserState { synErrorList     :: DS.Seq MyParseError
                        , symbolTable      :: SymbolTable
                        , sTableErrorList  :: DS.Seq MyTypeError
                        }
          deriving(Show)

## Tipos

Para modelar los tipos del lenguaje usamos un tipo algebraico recursivo de *haskell*.

    data Type = MyInt
              | MyFloat
              | MyBool
              | MyChar
              | MyFunction  [Type] Type
              | MyProcedure [Type] 
              | MyError
              | MyEmpty
              | MyString
              | MyArray 
                  { getType :: Type
                  , getTam  :: Integer
                  }

Hemos definido un constructor por cada tipo que el usuario podrá usar dentro del lenguaje, además de otros que nos serviran para uso interno del compilador. A continuación, pasaremos a describir cada uno de estos:

* **MyInt**: Equivalente al tipo *int* del lenguaje.
* **MyFloat**: Equivalente al tipo *double* del lenguaje.
* **MyBool**: Equivalente al tipo *boolean* del lenguaje.
* **MyChar**: Equivalente al tipo *char* del lenguaje.
* **MyFunction**: Es usado para representar a la definición de una función. Esta compuesto por una lista de *type* que almacena en órden los tipos de cada uno de los parámetros de entrada, y por otro *type* que representa el tipo de salida de la función.
* **MyProcedure**: Usado para describir los procedimientos en nuestro lenguaje. Esta compuesto por una lista de *type* que representa los tipos de cada uno de los parámetros de entrada en órden.
* **MyError**: Usado para representar aquellos casos en los que la verificación de tipos de algún nodo del árbol no es correcta y hace falta expandir el error en el resto de la verificación.
* **MyString**: Usado para representar el tipo *string* del lenguaje.
* **MyArray**: Describe el tipo *array* del lenguaje. Contiene el tipo de los valores almacenados en el arreglo y la cantidad de estos.

## Errores


