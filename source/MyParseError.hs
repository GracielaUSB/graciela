module MyParseError where

import qualified Text.Parsec.Pos as P
import Location
import Token

data MyParseError = MyParseError   { loc       :: Location
                                   , waitedTok :: WaitedToken
                                   , actualTok :: Token
                                   }
                  | EmptyError     { loc :: Location }
                  | ScopesError
                  | NonAsocError   { loc :: Location }
               deriving (Read)

data WaitedToken =  Operator
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
                  deriving(Read)

instance Show WaitedToken where
  show Operator   = "operador"
  show Number     = "numero"
  show TokenRP    = "paréntesis derecho"
  show Comma      = "coma"
  show Final      = "final de archivo"
  show TokenRB    = "corchete derecho"
  show TokenOB    = "apertura de bloque"
  show TokenCB    = "final de bloque"
  show Program    = "program"
  show ProcOrFunc = "procedimiento o funcion"
  show Colon      = "dos puntos"
  show IDError    = "identificador"

instance Show MyParseError where
  show (MyParseError loc wt at)      = show loc ++ ": Esperaba " ++ show wt ++ " en vez de " ++ show at
  show (EmptyError   loc)            = show loc ++ ": No se permiten expresiones vacías"
  show ScopesError                   = "Error en la tabla de símbolos: intento de salir de un alcance sin padre"
  show (NonAsocError loc)            = show loc ++ " Operador no asociativo"
       
newEmptyError  pos          = EmptyError   { loc = Location (P.sourceLine pos) (P.sourceColumn pos) (P.sourceName pos)                                 }
newParseError  msg (e, pos) = MyParseError { loc = Location (P.sourceLine pos) (P.sourceColumn pos) (P.sourceName pos), waitedTok = msg, actualTok = e }
