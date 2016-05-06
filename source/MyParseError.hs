module MyParseError where
--------------------------------------------------------------------------------
import Location
import Token
--------------------------------------------------------------------------------
import  Text.Parsec.Pos (SourcePos, sourceLine, sourceColumn, sourceName)
--------------------------------------------------------------------------------

data MyParseError
    = MyParseError
        { loc         :: Location
        , expectedTok :: ExpectedToken
        , actualTok   :: Token
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
    =  Operator
    | Number
    | TokenRP
    | TokenRB
    | Comma
    | Final
    | Program
    | TokenOB
    | TokenCB
    | ProcOrFunc
    | Colon
    | IDError
    | Begin
    | LexEnd
    | TokenFunc
    | Arrow
    | TokenLP
    | TokenOA
    | TokenCA
    | TokenArg
    | SColon
    | Action
    | GuardSep
    | TokenIF
    | TokenFI
    | TokenAs
    | TokenDO
    | TokenOD
    | Cuant
    | Pipe
    | RightPer
    | AssignOrColon

instance Show ExpectedToken where
    show Operator      = "Operador"
    show Number        = "Número"
    show TokenRP       = "Paréntesis Derecho"
    show TokenLP       = "Paréntesis Iquierdo"
    show Comma         = "Coma"
    show Final         = "Final de Archivo"
    show TokenRB       = "Corchete Derecho"
    show TokenOB       = "Apertura de Bloque"
    show TokenCB       = "Final de Bloque"
    show Program       = "Program"
    show ProcOrFunc    = "Procedimiento o Función"
    show Colon         = "Dos puntos"
    show IDError       = "Identificador"
    show Begin         = "begin"
    show LexEnd        = "Token end"
    show TokenFunc     = "Token func"
    show Arrow         = "Token ->"
    show TokenCA       = "Token representante de final de aserción"
    show TokenOA       = "Token representande de inicio de aserción"
    show TokenArg      = "Token representante de clase de argumento"
    show SColon        = "punto y coma (;)"
    show Action        = "acción"
    show GuardSep      = "separador de guardias"
    show TokenIF       = "if"
    show TokenFI       = "fi"
    show TokenAs       = ":="
    show TokenDO       = "do"
    show TokenOD       = "od"
    show Cuant         = "cuantificador"
    show Pipe          = "barra vertical"
    show RightPer      = "cierre de cuantificador"
    show AssignOrColon = "asignación o dos puntos"


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
    MyParseError { loc = toLocation pos, expectedTok = msg, actualTok = e }
