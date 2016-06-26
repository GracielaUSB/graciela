module MyParseError where
--------------------------------------------------------------------------------
import Location
import Token
--------------------------------------------------------------------------------
import Text.Parsec.Pos (SourcePos, sourceLine, sourceColumn, sourceName)
--------------------------------------------------------------------------------

data MyParseError
    = MyParseError
        { loc         :: Location
        , expectedTok :: ExpectedToken
        , currentTok  :: Token
        }
    | EmptyError
        { loc       :: Location
        }
    | ArrayError
        { waDim     :: Int
        , prDim     :: Int
        , loc       :: Location
        }
    | NonAsocError
        { loc       :: Location
        }
    | ScopesError


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
    | IDError
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
    |TokenType

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
    show IDError       = "identificador"
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
        errorL loc ++ ": Esperaba " ++ show wt ++ " en vez de " ++
        show at ++ "."
    show (EmptyError   loc)       =
        errorL loc ++ ": No se permiten Expresiones vacías."
    show (NonAsocError loc)       =
        errorL loc ++ ": Operador no asociativo."
    show (ArrayError   wt pr loc) =
        errorL loc ++ ": Esperaba Arreglo de dimensión " ++ show wt ++
        ", encontrado Arreglo de dimensión " ++ show pr ++ "."
    show ScopesError              =
        "Error en la tabla de símbolos: intento de salir de un alcance sin padre."


newEmptyError :: SourcePos -> MyParseError
newEmptyError pos =
    EmptyError { loc = toLocation pos }


newParseError :: ExpectedToken -> (Token, SourcePos) -> MyParseError
newParseError msg (e, pos) =
    MyParseError { loc = toLocation pos, expectedTok = msg, currentTok = e }
