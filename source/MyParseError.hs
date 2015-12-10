module MyParseError where

import qualified Text.Parsec.Pos as P
import Location
import Token


data MyParseError = MyParseError   { loc       :: Location
                                   , waitedTok :: WaitedToken
                                   , actualTok :: Token
                                   }
                  | EmptyError     { loc       :: Location 
                                   }
                  | ArrayError     { waDim     :: Int
                                   , prDim     :: Int
                                   , loc       :: Location 
                                   }
                  | NonAsocError   { loc       :: Location 
                                   }
                  | ScopesError
                  

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
                  | Begin
                  | LexEnd
                  | TokenFunc
                  | Arrow
                  | TokenLP
                  | TokenOA
                  | TokenCA
                  | TokenArg

instance Show WaitedToken where
  show Operator   = "Operador"
  show Number     = "Número"
  show TokenRP    = "Paréntesis Derecho"
  show TokenLP    = "Paréntesis Iquierdo"
  show Comma      = "Coma"
  show Final      = "Final de Archivo"
  show TokenRB    = "Corchete Derecho"
  show TokenOB    = "Apertura de Bloque"
  show TokenCB    = "Final de Bloque"
  show Program    = "Program"
  show ProcOrFunc = "Procedimiento o Función"
  show Colon      = "Dos puntos"
  show IDError    = "Identificador"
  show Begin      = "Begin"
  show LexEnd     = "Token end"
  show TokenFunc  = "Token func"
  show Arrow      = "Token ->"
  show TokenCA    = "Token representante de final de aserción"
  show TokenOA    = "Token representande de inicio de aserción"
  show TokenArg   = "Token representante de clase de argumento"

instance Show MyParseError where
  show (MyParseError loc wt at) = 
      errorL loc ++ ": Esperaba " ++ show wt ++ " en vez de " ++ show at ++ "."
  show (EmptyError   loc)       = 
      errorL loc ++ ": No se permiten Expresiones vacías."
  show (NonAsocError loc)       = 
      errorL loc ++ ": Operador no asociativo."
  show (ArrayError   wt pr loc) = 
      errorL loc ++ ": Esperaba Arreglo de dimensión " ++ show wt ++ ", encontrado Arreglo de dimensión " ++ show pr ++ "."     
  show ScopesError              = 
      "Error en la tabla de símbolos: intento de salir de un alcance sin padre."
    

newEmptyError :: P.SourcePos -> MyParseError
newEmptyError  pos          = 
    EmptyError   { loc = Location (P.sourceLine pos) (P.sourceColumn pos) (P.sourceName pos) }


newParseError :: WaitedToken -> (Token, P.SourcePos) -> MyParseError
newParseError  msg (e, pos) = 
    MyParseError { loc = Location (P.sourceLine pos) (P.sourceColumn pos) (P.sourceName pos), 
                                                                waitedTok = msg, actualTok = e }


checkErrorPosP :: MyParseError -> MyParseError -> Ordering
checkErrorPosP x y = getFirstLoc (loc x) (loc y)  
