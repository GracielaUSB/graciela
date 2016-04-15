{-|
Module      : Token
Description : Todos los tokens del lenguaje
Copyright   : GraCieLa

Posee todo lo referente a tokens generados por el lexer, que seran
luego utilizados por el parser 
-}
module Token where

import qualified Data.Text as T
import Text.Parsec
import Type


-- | Rpresenta una tupla con un token y su posicion
type TokenPos = (Token, SourcePos)


-- | Representa a todos las palabras reservadas en el lenguaje
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
             | TokAsig
             | TokLess
             | TokGreater
             | TokNot 
             | TokProgram  
             | TokOpenBlock
             | TokCloseBlock
             | TokLeftBracket
             | TokRightBracket
             | TokSemicolon
             | TokColon
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
             | TokFunc      
             | TokProc     
             | TokIn      
             | TokOut 
             | TokInOut  
             | TokRef     
             | TokWith             
             | TokMod    
             | TokMax         
             | TokMin      
             | TokForall      
             | TokExist    
             | TokNotExist   
             | TokSigma      
             | TokPi             
             | TokIf         
             | TokFi            
             | TokDo         
             | TokOd                 
             | TokAbs       
             | TokSqrt      
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
             | TokError Char
             | TokBegin
             | TokLexEnd
             | TokComment
             | EmptyToken
      deriving (Eq)


-- | Instancia 'Show' para los tokens
instance Show Token where
  show TokBegin         = "\"begin\" - Inicio de Procedimiento o Función"
  show TokLexEnd        = "\"end\" - Fin de Procedimiento o Función"
  show TokPlus          = "\"+\" - Suma"
  show TokMinus         = "\"-\" - Resta"
  show TokStar          = "\"*\" - Multiplicación"   
  show TokSlash         = "\"/\" - División"        
  show TokEnd           = "\"end\" - Fin de Archivo"
  show TokComma         = "\",\" - Coma"
  show TokLeftParent    = "\"(\" - Paréntesis Izquierdo"
  show TokRightParent   = "\")\" - Paréntesis Derecho"
  show TokLeftPercent   = "\"(%\" - Apertura de Cuantificador"
  show TokRightPercent  = "\"%)\" - Cierre de Cuantificador"
  show TokAccent        = "\"^\" - Potencia"
  show TokLogicalAnd    = "\"/\\\" - Conjunción Lógica"
  show TokLogicalOr     = "\"\\/\" - Disyunción Lógica"
  show TokNotEqual      = "\"!=\" - Inequivalencia"
  show TokLessEqual     = "\"<=\" - Menor o Igual que"
  show TokGreaterEqual  = "\">=\" - Mayor o Igual que"
  show TokImplies       = "\"==>\" - Implicación"
  show TokConsequent    = "\"<==\" - Consecuencia"
  show TokEquiv         = "\"==\" - Equivalencia"
  show TokAsig          = "\":=\" - Asignación"
  show TokLess          = "\"<\" - Menor que"
  show TokGreater       = "\">\" - Mayor que"
  show TokNot           = "\"!\" - Negación"
  show TokProgram       = "\"program\" - Inicio del Programa"
  show TokLeftBracket   = "\"[\" - Corchete Izquierdo"
  show TokRightBracket  = "\"]\" - Corchete Derecho"
  show TokSemicolon     = "\";\" - Punto y Coma"
  show TokColon         = "\":\" - Dos Puntos"
  show TokLeftBrace     = "\"{\" - Llave Izquierda"
  show TokRightBrace    = "\"}\" - Llave Derecho"
  show TokArrow         = "\"->\" - Flecha"
  show TokLeftPre       = "\"{pre\" - Apertura de Precondición"
  show TokRightPre      = "\"pre}\" - Cierre de Precondición"
  show TokLeftPost      = "\"{post\" - Apertura de Postcondición"
  show TokRightPost     = "\"post}\" - Cierre de Postcondición"
  show TokLeftBound     = "\"{bound\" - Apertura de la Función de Cota"
  show TokRightBound    = "\"bound}\" - Cierre de la Función de Cota"
  show TokLeftA         = "\"{a\" - Apertura de Aserción"
  show TokRightA        = "\"a}\" - Cierre de Aserción"
  show TokLeftInv       = "\"{inv\" - Apertura de Invariante"
  show TokRightInv      = "\"inv}\" - Cierre de Invariante"
  show TokFunc          = "\"func\" - Función"
  show TokProc          = "\"proc\" - Procedimiento"
  show TokIn            = "\"in\" - Parámetro de Entrada"
  show TokOut           = "\"out\" - Parámetro de Salida"
  show TokInOut         = "\"inout\" - Parámetro de Entrada y Salida"
  show TokRef           = "\"ref\" - Parámetro por Referencia"
  show TokWith          = "\"with\" - Indicador de Archivo de Entrada"
  show TokMod           = "\"mod\" - Modulo"
  show TokMax           = "\"max\" - Máximo"
  show TokMin           = "\"min\" - Mínimo"
  show TokForall        = "\"forall\" - Para Todo"
  show TokExist         = "\"exist\" - Existencial"
  show TokSigma         = "\"sigma\" - Sumatioria"
  show TokPi            = "\"pi\" - Productoria"
  show TokIf            = "\"if\" - Apertura de Selector"
  show TokFi            = "\"fi\" - Cierre de Selector"
  show TokDo            = "\"do\" - Apertura de Repetidor"
  show TokOd            = "\"od\" - Cierre de Repetidor"
  show TokAbs           = "\"abs\" - Valor Absoluto"
  show TokSqrt          = "\"sqrt\" - Raíz Cuadrada"
  show TokVar           = "\"var\" - Definidor de Variables"
  show TokConst         = "\"const\" - Definidor de Constantes"
  show TokAbort         = "\"abort\" - Abortador de Programa"
  show TokRandom        = "\"random\" - Random"
  show TokSkip          = "\"skip\" - Instrucción de Salto"
  show TokWrite         = "\"write\" - Escritor"
  show TokWriteln       = "\"writeln\" - Escritor con Salto de Línea"
  show TokRead          = "\"read\" - Lector"
  show TokToInt         = "\"toInt\" - Conversión a Entero"
  show TokToDouble      = "\"toDouble\" - Conversión a Flotante"
  show TokToChar        = "\"toChar\" - Conversión a Caracter"        
  show TokMIN_INT       = "\"MIN_INT\" - Mínimo Entero"
  show TokMIN_DOUBLE    = "\"MIN_DOUBLE\" - Mínimo Flotante"
  show TokMAX_INT       = "\"MAX_INT\" - Máximo Entero" 
  show TokMAX_DOUBLE    = "\"MAX_DOUBLE\" - Máximo Flotante"
  show TokOf            = "\"of\" - of del Arreglo"
  show (TokBool     b)  = "\"" ++ showBool b ++ "\" - Booleano"
  show (TokType     t)  = "\"" ++ show t ++ "\" - Tipo " ++ showType t
  show (TokInteger  n)  = "\"" ++ show n ++ "\" - Entero"
  show (TokFlotante n)  = " \"" ++ show n ++ "\" - Flotante"
  show (TokChar     c)  = "" ++ show c ++ " - Caracter"
  show (TokId       i)  = "" ++ show i ++ " - Variable"
  show (TokString   e)  = "" ++ show e ++ " - Cadena de Caracteres"
  show (TokError    e)  = show e ++ " - Caracter no Permitido" 
  show (TokOpenBlock)   = "\"|[\" - Apertura de Bloque"
  show (TokCloseBlock)  = "\"]|\" - Cierre de Bloque"
  show TokSepGuards     = "\"[]\" - Separador de Guardias"
  show TokArray         = "\"array\" - Tipo Arreglo" 
  show TokPipe          = "\"|\" - Barra Vertical" 
  show TokComment       = "\"//\" - Comentatios" 


-- | Se encarga de imprimir los boleanos
showBool :: Bool -> String
showBool True  = "true"
showBool False = "false"


-- | Se encarga de imprimir los tipos
showType :: Type -> String
showType MyInt   = "Entero "
showType MyFloat = "Flotante "
showType MyChar  = "Caracter "
showType MyBool  = "Booleano "


-- | Retorna el token de una tupla 'TokenPos'
getToken :: TokenPos -> Token
getToken (token, _) = token


-- | Retorna la posicion de una tupla 'TokenPos'
getPos :: TokenPos -> SourcePos
getPos (_, pos) = pos


-- | Retorna el entero asociado a un token
tokenToInt :: Token -> Maybe Integer
tokenToInt (TokInteger n) = return n
tokenToInt _              = Nothing

-- | Retorna el flotante asociado a un token
tokenToDouble :: Token -> Maybe Double
tokenToDouble (TokFlotante n) = return n
tokenToDouble _               = Nothing


-- | Retorna el boleano asociado a un token
tokenToBool :: Token -> Maybe Bool
tokenToBool (TokBool b) = return b
tokenToBool _           = Nothing


-- | Retorna el caracter asociado a un token
tokenToChar :: Token -> Maybe Char
tokenToChar (TokChar c) = return c
tokenToChar _           = Nothing


-- | Retorna la lista de caracteres asociado a un token
tokenToString :: Token -> Maybe String
tokenToString (TokString s) = return s
tokenToString _             = Nothing