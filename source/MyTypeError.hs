module MyTypeError where
--------------------------------------------------------------------------------
import           AST
import           Contents      as C
import           Data.Monoid
import           Location
import           Type
--------------------------------------------------------------------------------
import           Data.Foldable (foldl, toList)
import           Data.Function (on)
import           Data.List     hiding (sortBy)
import           Data.Sequence (Seq)
import           Data.Sequence as Seq (sortBy, take)
import           Data.Text     (Text)
import           Prelude       hiding (foldl)
--------------------------------------------------------------------------------

data MyTypeError
    = RepSymbolError
        { symbol :: Text
        , preLoc :: Location
        , loc    :: Location
        }
    | ConstIdError
        { symbol :: Text
        , loc    :: Location
        }
    | NonDeclError
        { symbol :: Text
        , loc    :: Location
        }
    | ArithmeticError
        { ltype :: Type
        , rtype :: Type
        , arOp  :: OpNum
        , loc   :: Location
        }
    | BooleanError
        { ltype :: Type
        , rtype :: Type
        , boOp  :: OpBool
        , loc   :: Location
        }
    | RelationalError
        { ltype :: Type
        , rtype :: Type
        , reOp  :: OpRel
        , loc   :: Location
        }
    | UnaryError
        { prType :: Type
        , unOp   :: OpUn
        , loc    :: Location
        }
    | StateError
        { prType :: Type
        , state  :: StateCond
        , loc    :: Location
        }
    | GuardError
        { prType :: Type
        , loc    :: Location
        }
    | CondError
        { loc :: Location
        }
    | IncomDefError
        { cont :: VarBehavior
        , loc  :: Location
        }
    | UndecFunError
        { symbol :: Text
        , isFunc :: Bool
        , loc    :: Location
        }
    | NumberArgsError
        { symbol :: Text
        , isFunc :: Bool
        , wtLeng :: Int
        , prLeng :: Int
        , loc    :: Location
        }
    | RetFuncError
        { symbol :: Text
        , waType :: Type
        , prType :: Type
        , loc    :: Location
        }
    | FunArgError
        { symbol :: Text
        , isFunc :: Bool
        , waType :: Type
        , prType :: Type
        , loc    :: Location
        }
    | AssignError
        { aeName :: Text
        , waType :: Type
        , prType :: Type
        , loc    :: Location
        }
    | ArrayCallError
        { arrName:: Text
        , prType  :: Type
        , loc     :: Location
        }
    | ArrayDimError
        { arrName:: Text
        , waDim   :: Int
        , prDim   :: Int
        , loc     :: Location
        }
    | IntOutOfBounds
        { val :: Text
        , loc :: Location
        }

    | RanError
        { symbol :: Text
        , prType :: Type
        , loc    :: Location
        }
    | UncountError
        { op  :: OpQuant
        , loc :: Location
        }
    | NotOccursVar
        { op     :: OpQuant
        , symbol :: Text
        , loc    :: Location
        }
    | FunNameError
        { symbol :: Text
        , loc    :: Location
        }
    | InvalidPar
        { name :: Text
        , tree :: AST Type
        , loc  :: Location
        }
    | DiffSizeError
        { loc :: Location
        }
    | TypeDecError
        { symbol :: Text
        , loc    :: Location
        , tyExp  :: Type
        , tyVar  :: Type
        }
    | QuantRangeError
        { op     :: OpQuant
        , trange :: Type
        , loc    :: Location
        }
    | QuantIntError
        { op    :: OpQuant
        , tterm :: Type
        , loc   :: Location
        }
    | QuantBoolError
        { op    :: OpQuant
        , tterm :: Type
        , loc   :: Location
        }
    | NotIntError
        { symbol :: Text
        , loc    :: Location
        }
    | NotConstError
        { symbol :: Text
        , loc    :: Location
        }
    | NotInitError
        { symbol :: Text
        , loc    :: Location
        }
    | NotRValueError
        { symbol :: Text
        , loc    :: Location
        }
    | IntError
        { symbol :: Text
        , loc    :: Location
        }
    deriving (Eq)


instance Show MyTypeError where
    show e = errorL (loc e) ++ case e of
        (RepSymbolError     sym pLoc _) ->
            ": La variable "  ++ show sym ++
            " ya fue previamente declarada " ++ show pLoc ++ "."
        (ConstIdError       sym      _) ->
            ": No se puede cambiar el valor de la constante " ++
            show sym ++ "."
        (NonDeclError       sym      _) ->
            ": La variable " ++ show sym ++ " no está declarada."
        (ArithmeticError    lt rt op _) ->
            ": Tipos incompatibles en el operador aritmético " ++
            show op ++ ", se suministró el tipo " ++ show lt ++ " & " ++
            show rt ++ "."
        (BooleanError       lt rt op _) ->
            ": Tipos incompatibles en el operador booleano " ++
            show op ++ ", se suministró el tipo " ++ show lt ++ " & " ++
            show rt ++ "."
        (RelationalError    lt rt op _) ->
            ": Tipos incompatibles en el operador relacional " ++
            show op ++ ", se suministró el tipo " ++ show lt ++ " & " ++
            show rt ++ "."
        (UnaryError          t Minus _) ->
            ": Tipo incompatible en el operador unario Negativo, se suministró el tipo " ++
            show t ++ ", se esperaba el tipo int o double."
        (UnaryError          t Not   _) ->
            ": Tipo incompatible en el operador unario Negación, se suministró el tipo " ++
            show t ++ ", se esperaba el tipo boolean."
        (UnaryError          t    op _) ->
            ": Tipo incompatible en el operador unario " ++
            show op ++", se suministró el tipo " ++ show t ++
            ", se esperaba el tipo int."
        (StateError          t Bound _) ->
            ": Se esperaba en la Función de Cota una expresión de tipo int."
        (StateError          t s     _) ->
            ": Se esperaba en la " ++ show s ++
            " una expresión de tipo boolean."
        (GuardError          t       _) ->
            ": Se esperaba un tipo boolean en la guardia, se suministró " ++
            show t ++ "."
        (CondError                   _) ->
            ": Los condicionales no son del mismo tipo."
        (IncomDefError C.Constant    _) ->
            ": Definición de constantes incompleta."
        (IncomDefError C.Variable    _) ->
            ": Definición de variables incompleta."
        (UndecFunError   sym  True   _) ->
            ": La función " ++ show sym ++
            " no se puede usar, no esta definida."
        (UndecFunError   sym  False  _) ->
            ": El procedimiento " ++ show sym ++
            " no se puede usar, no esta definido."
        (NumberArgsError sym True wtL prL _) ->
            ": El número de argumentos de la función " ++
            show sym ++ " es inválido, se esperaba " ++ show prL ++
            " argumentos, se suministró " ++ show wtL ++ "."
        (NumberArgsError sym False wtL prL _) ->
            ": El número de argumentos del procedimiento " ++
            show sym ++ " es inválido, se esperaba " ++ show prL ++
            " argumentos, se suministró " ++ show wtL ++ "."
        (RetFuncError    sym wt  pt  _) ->
            ": En la función " ++ show sym ++
            " se esperaba que retornará el tipo " ++ show wt ++
            ", se suministró " ++ show pt ++ "."
        (FunArgError sym True  wt pt _) ->
            ":La función " ++ show sym ++
            " esperaba un argumento de tipo " ++ show wt ++
            ", se suministró " ++ show pt ++ "."
        (FunArgError sym False wt pt _) ->
            ":El procedimiento " ++ show sym ++
            " esperaba un argumento de tipo " ++ show wt ++
            ", se suministró " ++ show pt ++ "."
        (AssignError       sym wt pt _) ->
            ": La Variable " ++ show sym ++
            " es de tipo " ++ show wt ++ ", se suministró " ++ show pt ++ "."
        (ArrayDimError     sym wd pd _) ->
            ": El Arreglo " ++ show sym ++
            " es de dimensión " ++ show wd ++ ", se suministró dimensión " ++
            show pd ++ "."
        (ArrayCallError    sym    pt _) ->
            ": El índice del arreglo " ++ show sym ++
            " debe ser de tipo int, se suministró " ++ show pt ++ "."
        (IntOutOfBounds          val _) ->
            ": int " ++ show val ++" fuera del rango representable."
        (RanError          sym    pt _) ->
            ": La variable" ++ show sym ++ " es de tipo " ++
            show pt ++ ", se esperaba varible de tipo int o double."
        (UncountError  op            _) ->
            ": Tipo no contable en la definición del Cuantificador " ++
            show op ++ "."
        (NotOccursVar  op  sym       _) ->
            ": La varible " ++ show sym ++
            " no ocurre dentro del rango del Cuantificador " ++ show op ++ "."
        (FunNameError  id            _) ->
            ": El parámetro " ++ show id ++
            " es del mismo nombre de la función que está siendo definida."
        (InvalidPar  name _         _) ->
            ": En el procedimiento " ++ show name ++
            " se suministrado una constante como parámetro de salida."
        (DiffSizeError                _) ->
            ": El número de variables declaradas es distinto al de expresiones de inicialización encontradas."
        (TypeDecError  id loc te tv) ->
            ": La variable " ++ show id ++ " es del tipo " ++ show tv ++
            " pero su expresión correspondiente es del tipo " ++ show te ++ "."
        (QuantRangeError op tr _) ->
            ": El Cuantificador " ++ show op ++
            " esperaba un rango del tipo boolean, se suministró " ++
            show tr ++ "."
        (QuantIntError  op tt _) ->
            ": El Cuantificador " ++ show op ++
            " esperaba un término del tipo int o double, se suministró " ++
            show tt ++ "."
        (QuantBoolError  op tt _) ->
            ": El Cuantificador " ++ show op ++
            " esperaba un término del tipo boolean, se suministró " ++
            show tt ++ "."
        (NotConstError  id            _) ->
            ": La variable " ++ show id ++ " no es constante."
        (NotIntError  id            _) ->
            ": La variable " ++ show id ++ " no es del tipo int"
        (NotInitError  id           _) ->
            ": La variable " ++ show id ++ " no está inicializada."
        (NotRValueError  id           _) ->
            ": La variable " ++ show id ++ " no es un r-value."
        (IntError  id           _) ->
            ": La variable " ++ show id ++ " no es del tipo int."

drawTypeError n =
    unlines . map show . toList . take' n . Seq.sortBy (compare `on` loc)

take' :: Maybe Int -> Seq a -> Seq a
take' Nothing  = Prelude.id
take' (Just x) = Seq.take x
