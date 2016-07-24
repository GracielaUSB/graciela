module MyParseError where
--------------------------------------------------------------------------------
import Token
--------------------------------------------------------------------------------
import Text.Megaparsec.Pos (SourcePos (..))
--------------------------------------------------------------------------------

data MyParseError
    = MyParseError
        { loc         :: SourcePos
        , expectedTok :: ExpectedToken
        , currentTok  :: Token
        }
    | EmptyError
        { loc       :: SourcePos
        }
    | ArrayError
        { waDim     :: Int
        , prDim     :: Int
        , loc       :: SourcePos
        }
    | NonAsocError
        { loc       :: SourcePos
        }
    | ScopesError

    | CustomError -- Mientras no mejoremos los errores jajaja
        { msg :: String
        , loc :: SourcePos
        }


data ExpectedToken
    = Action
    | Arrow
    | AssignOrColon
    | Begin
    | Colon
    | Comma
    | Cuant
    | Final
    | GuardSep
    | IdError
    | Implements
    | LexEnd
    | Number
    | Operator
    | Pipe
    | ProcOrFunc
    | Program
    | RightPer
    | SColon
    | TokenArg
    | TokenAs
    | TokenCA
    | TokenCB
    | TokenFI
    | TokenFunc
    | TokenIF
    | TokenLP
    | TokenLB
    | TokenOA
    | TokenOB
    | TokenOD
    | TokenRB
    | TokenRP
    | TokEOFO
    | TokenType


instance Show ExpectedToken where
    show Action        = "acción"
    show Arrow         = "token '->'"
    show AssignOrColon = "token ':=' o ':'"
    show Begin         = "'begin'"
    show Colon         = "token ':' "
    show Comma         = "token ','"
    show Cuant         = "cuantificador"
    show Final         = "Final de Archivo"
    show GuardSep      = "separador de guardias"
    show IdError       = "identificador"
    show Implements    = "implements"
    show LexEnd        = "token 'end'"
    show Number        = "número"
    show Operator      = "operador"
    show Pipe          = "barra vertical"
    show ProcOrFunc    = "Procedimiento o Función"
    show Program       = "Program"
    show RightPer      = "cierre de cuantificador"
    show SColon        = "token ';'"
    show TokenArg      = "token representante de clase de argumento"
    show TokenAs       = ":="
    show TokenCA       = "token representante de final de aserción"
    show TokenCB       = "Final de Bloque"
    show TokenFI       = "fi"
    show TokenFunc     = "token func"
    show TokenIF       = "if"
    show TokenLP       = "token '('"
    show TokenLB       = "token ']'"
    show TokenOA       = "token representante de inicio de aserción"
    show TokenOB       = "token '|['"
    show TokenOD       = "od"
    show TokenRB       = "token ']'"
    show TokenRP       = "token ')'"
    show TokEOFO       = "do"
    show TokenType     = "un tipo"


instance Show MyParseError where
    show (MyParseError loc wt at) =
        "Error " ++ show loc ++ ": Esperaba " ++ show wt ++ " en vez de " ++
        show at ++ "."
    show (EmptyError   loc)       =
        "Error " ++ show loc ++ ": No se permiten Expresiones vacías."
    show (NonAsocError loc)       =
        "Error " ++ show loc ++ ": Operador no asociativo."
    show (ArrayError   wt pr loc) =
        "Error " ++ show loc ++ ": Esperaba Arreglo de dimensión " ++ show wt ++
        ", encontrado Arreglo de dimensión " ++ show pr ++ "."
    show ScopesError              =
        "Error en la tabla de símbolos: intento de salir de un alcance sin padre."
    show (CustomError msg loc) =
        "Error " ++ show loc ++ ": " ++ msg



-- newEmptyError :: SourcePos -> MyParseError
-- newEmptyError pos =
--     EmptyError { loc = toSourcePos pos }


-- newParseError :: ExpectedToken -> (Token, SourcePos) -> MyParseError
-- newParseError msg (e, pos) =
--     MyParseError { loc = toSourcePos pos, expectedTok = msg, currentTok = e }
