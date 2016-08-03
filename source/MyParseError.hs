module MyParseError where
--------------------------------------------------------------------------------
import           Location
import           Token
import           Data.Monoid ((<>))
--------------------------------------------------------------------------------

data MyParseError
  = MyParseError
    { pos         :: SourcePos
    , expectedTok :: ExpectedToken
    , currentTok  :: Token
    }
  | EmptyError
    { pos :: SourcePos
    }
  | ArrayError
    { waDim :: Int
    , prDim :: Int
    , pos   :: SourcePos
    }
  | NonAsocError
    { pos :: SourcePos
    }
  | ScopesError
  | CustomError -- Mientras no mejoremos los errores jajaja
    { msg :: String
    , loc :: Location
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
  show (MyParseError pos wt at) =
    "Error " <> " " <> show pos <> ": Esperaba " <> show wt <> " en vez de " <>
    show at <> "."
  show (EmptyError   pos)       =
    "Error " <> " " <> show pos <> ": No se permiten Expresiones vacías."
  show (NonAsocError pos)       =
    "Error " <> " " <> show pos <> ": Operador no asociativo."
  show (ArrayError   wt pr pos) =
    "Error " <> " " <> show pos <> ": Esperaba Arreglo de dimensión " <> show wt <>
    ", encontrado Arreglo de dimensión " <> show pr <> "."
  show ScopesError              =
    "Error en la tabla de símbolos: intento de salir de un alcance sin padre."
  show (CustomError msg loc) =
    "Error " <> " " <> show loc <> ": " <> msg


newParseError :: ExpectedToken -> (Token, SourcePos) -> MyParseError
newParseError msg (e, pos) =
    MyParseError { pos = pos, expectedTok = msg, currentTok = e }
