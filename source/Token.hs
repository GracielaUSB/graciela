module Token where

import qualified Data.Text as T
import Text.Parsec
import Data.Int
import Type

data TypeBool = MyTrue | MyFalse
  deriving (Show, Read, Eq) 


data Token =   TokPlus
             | TokMinus 
             | TokStar 
             | TokSlash 
             | TokSepGuards 
             | TokEnd 
             | TokComma 
             | TokLeftParent 
             | TokRightParent
             | TokLeftPercent
             | TokRightPercent
             | TokAccent
             | TokPipe
             | TokLogicalAnd
             | TokLogicalOr
             | TokNotEqual
             | TokLessEqual
             | TokGreaterEqual
             | TokImplies
             | TokConsequent
             | TokEquiv
             | TokNotEqiv
             | TokAsig
             | TokLess
             | TokGreater
             | TokEqual
             | TokNot 
             | TokProgram  
             | TokOpenBlock
             | TokCloseBlock
             | TokLeftBracket
             | TokRightBracket
             | TokVerticalBar
             | TokSemicolon
             | TokColon
             | TokOpenQuant
             | TokLeftBrace
             | TokRightBrace
             | TokArrow
             | TokLeftPre
             | TokRightPre
             | TokLeftPost
             | TokRightPost
             | TokLeftBound
             | TokRightBound
             | TokLeftA
             | TokRightA
             | TokLeftInv
             | TokRightInv  
             | TokPre        
             | TokPost      
             | TokBound 
             | TokFunc      
             | TokProc     
             | TokIn      
             | TokOut 
             | TokInOut       
             | TokWith             
             | TokMod    
             | TokMax         
             | TokMin      
             | TokForall      
             | TokExist    
             | TokNotExist   
             | TokSigma      
             | TokPi          
             | TokUnion      
             | TokIf         
             | TokFi        
             | TokInv       
             | TokDo         
             | TokOd         
             | TokGcd        
             | TokAbs       
             | TokSqrt      
             | TokLength     
             | TokVar      
             | TokConst     
             | TokAbort       
             | TokRandom      
             | TokSkip        
             | TokWrite    
             | TokWriteln
             | TokRead     
             | TokToInt      
             | TokToDouble   
             | TokToChar      
             | TokToString                
             | TokMIN_INT     
             | TokMIN_DOUBLE 
             | TokMAX_INT   
             | TokMAX_DOUBLE 
             | TokOf
             | TokArray
             | TokChar     Char
             | TokBool     Bool      
             | TokType     Type
             | TokId       T.Text
             | TokString   String
             | TokInteger  Integer
             | TokFlotante Double
             | TokError T.Text
             | EmptyToken
      deriving (Eq)


type TokenPos = (Token, SourcePos)

getToken :: TokenPos -> Token
getToken (token, _) = token

getPos :: TokenPos -> SourcePos
getPos (_, pos) = pos

instance Show Token where
  show TokPlus          = "suma"
  show TokMinus         = "resta"
  show TokStar          = "multiplicación"   
  show TokSlash         = "división"        
  show TokEnd           = "fin de archivo"
  show TokComma         = "coma"
  show TokLeftParent    = "paréntesis izquierdo"
  show TokRightParent   = "paréntesis derecho"
  show TokLeftPercent   = "cuantificador de apertura"
  show TokRightPercent  = "cuantificador de cierre"
  show TokAccent        = "potencia"
  show TokLogicalAnd    = "y lógico"
  show TokLogicalOr     = "o lógico"
  show TokNotEqual      = "inequivalencia"
  show TokLessEqual     = "menor o igual que"
  show TokGreaterEqual  = "mayor o igual que"
  show TokImplies       = "implicación"
  show TokConsequent    = "consecuencia"
  show TokEquiv         = "equivalencia"
  show TokNotEqiv       = "no equivalencia"
  show TokAsig          = "asignación"
  show TokLess          = "menor que"
  show TokGreater       = "mayor que"
  show TokEqual         = "igual" 
  show TokNot           = "negacion"
  show TokProgram       = "programa"
  show TokLeftBracket   = "corchete de apertura"
  show TokRightBracket  = "corchete de cierre"
  show TokVerticalBar   = "barra" 
  show TokSemicolon     = "punto y coma"
  show TokColon         = "punto"
  show TokLeftBrace     = "llave de apertura"
  show TokRightBrace    = "llave de cierre"
  show TokArrow         = "flecha de tipo de retorno"
  show TokLeftPre       = "precondición de apertura"
  show TokRightPre      = "precondición de cierre"
  show TokLeftPost      = "postcondición de apertura"
  show TokRightPost     = "postcondición de cierre"
  show TokLeftBound     = "cuota de apertura"
  show TokRightBound    = "cuota de cierre"
  show TokLeftA         = "absorción de apertura"
  show TokRightA        = "absorción de cierre"
  show TokLeftInv       = "invariante de apertura"
  show TokRightInv      = "invariante de cierre"
  show TokPre           = "precondición"
  show TokPost          = "postcondición"
  show TokBound         = "cuota"
  show TokFunc          = "función"
  show TokProc          = "procedimiento"
  show TokIn            = "entrada"
  show TokOut           = "salida"
  show TokInOut         = "entrada y salida"
  show TokWith          = "token with"   
  show TokMod           = "modulo"
  show TokMax           = "Maximo"
  show TokMin           = "Minimo"
  show TokForall        = "Para Todo"
  show TokExist         = "Existencial"
  show TokNotExist      = "No Existencial"
  show TokSigma         = "Sumatoria"
  show TokPi            = "Productoria"
  show TokUnion         = "Union"
  show TokIf            = "if de apertura"
  show TokFi            = "if de cierre"
  show TokInv           = "invariante"
  show TokDo            = "do de apertura"
  show TokOd            = "do de cierre"
  show TokGcd           = "máximo común divisor"
  show TokAbs           = "valor absoluto"
  show TokSqrt          = "raíz cuadrada"
  show TokLength        = "longitud"
  show TokVar           = "variable"
  show TokConst         = "constante"
  show TokAbort         = "abortar"
  show TokRandom        = "random"
  show TokSkip          = "saltar"
  show TokWrite         = "escribir"
  show TokWriteln       = "escribir con salto de línea"
  show TokRead          = "leer"
  show TokToInt         = "conversión a entero"
  show TokToDouble      = "conversión a flotante"
  show TokToChar        = "conversión a caracter"
  show TokToString      = "conversión a cadena de caracteres"          
  show TokMIN_INT       = "entero mínimo"
  show TokMIN_DOUBLE    = "flotante mínimo"
  show TokMAX_INT       = "entero máximo" 
  show TokMAX_DOUBLE    = "flotante máximo"
  show TokOf            = "token of"
  show (TokBool b)      = "booleano : " ++ show b
  show (TokType b)      = "tipo: " ++ show b
  show (TokInteger b)   = "numero : " ++ show b
  show (TokId      b)   = show b
  show (TokString  e)   = "cadena de caracteres " ++ show e
  show (TokError   e)   = "cadena no reconocida " ++ show e
  show (TokFlotante n)  = "Numero flotante : " ++ show n
  show (TokChar c)      = "caracter " ++ show c  
  show (TokOpenBlock)   = "simbolo de apertura de bloque"
  show (TokCloseBlock)  = "simbolo de cierre de bloque"
  show TokSepGuards     = "simbolo separador de guardias"
  show TokArray         = "tipo arreglo de"
  show TokOpenQuant     = "apertura de cuantificador"
  show TokPipe          = "pipe"

tokenToInt :: Token -> Maybe Integer
tokenToInt (TokInteger n) = return n
tokenToInt _              = Nothing

tokenToDouble :: Token -> Maybe Double
tokenToDouble (TokFlotante n) = return n
tokenToDouble _               = Nothing

tokenToBool :: Token -> Maybe Bool
tokenToBool (TokBool b) = return b
tokenToBool _           = Nothing

tokenToChar :: Token -> Maybe Char
tokenToChar (TokChar c) = return c
tokenToChar _           = Nothing

tokenToString :: Token -> Maybe String
tokenToString (TokString s) = return s
tokenToString _             = Nothing
