{-# LANGUAGE LambdaCase #-}
{-|
Module      : Token
Description : Todos los tokens del lenguaje
Copyright   : GraCieLa

Posee todo lo referente a tokens generados por el lexer, que seran
luego utilizados por el parser
-}
module Token where
--------------------------------------------------------------------------------
import           Type
--------------------------------------------------------------------------------
import           Data.Text   (Text)
import           Text.Parsec (SourcePos)
--------------------------------------------------------------------------------
-- TokenPos ----------------------------

-- | Representa una tupla con un token y su posicion
type TokenPos = (Token, SourcePos)

-- | Retorna el token de una tupla 'TokenPos'
getToken :: TokenPos -> Token
getToken = fst

-- | Retorna la posicion de una tupla 'TokenPos'
getPos :: TokenPos -> SourcePos
getPos = snd

-- Tokens ------------------------------
-- | Representa a todos las palabras reservadas en el lenguaje
data Token
    = TokProgram
    | TokBegin
    | TokEnd
    | TokEOF

    | TokFunc
    | TokProc
    | TokIn
    | TokOut
    | TokInOut
    | TokRef

    | TokWith

    | TokVar
    | TokConst
    | TokOf

    | TokAsig

    | TokPlus
    | TokMinus
    | TokTimes
    | TokDiv
    | TokMod
    | TokPower

    | TokAbs
    | TokSqrt

    | TokComma
    | TokColon
    | TokSemicolon
    | TokArrow

    | TokEQ
    | TokNE
    | TokLE
    | TokGE
    | TokLT
    | TokGT

    | TokAnd
    | TokOr
    | TokNot

    | TokImplies
    | TokConsequent

    | TokLeftPar
    | TokRightPar

    | TokLeftPercent
    | TokRightPercent

    | TokLeftBracket
    | TokRightBracket

    | TokLeftBrace
    | TokRightBrace

    | TokOpenBlock
    | TokCloseBlock

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
    | TokPipe

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

    | TokSepGuards

    | TokAbort
    | TokSkip

    | TokRandom
    | TokWrite
    | TokWriteln
    | TokRead

    | TokToInt
    | TokToDouble
    | TokToChar

    | TokMinInt
    | TokMinDouble
    | TokMaxInt
    | TokMaxDouble

    | TokBool       { unTokBool :: Bool }
    | TokChar       { unTokChar :: Char }
    | TokInteger    { unTokInteger :: Integer }
    | TokFloat      { unTokFloatante :: Double}
    | TokString     { unTokString :: String }

    | TokArray
    | TokType       { unTokType :: Type}

    | TokId         { unTokId :: Text}

    | TokComment
    | EmptyToken
    | TokUnexpected { unTokUnexpected :: Char}

    -- V2.0
    | TokDataType      -- Ya hay un toktype :(, no soy creativo
    | TokImplements
    | TokAbstract
    | TokLeftRep    
    | TokRightRep   
    | TokLeftAcopl    
    | TokRightAcopl   


    deriving (Eq)


-- | Instancia 'Show' para los tokens
instance Show Token where 
    show = \case
        TokProgram        -> "\"program\" - Inicio del Programa"
        TokBegin          -> "\"begin\" - Inicio de Procedimiento o Función"
        TokEnd            -> "\"end\" - Fin de Procedimiento o Función"
        TokEOF            -> "\"EOF\" - Fin de Archivo"

        TokFunc           -> "\"func\" - Función"
        TokProc           -> "\"proc\" - Procedimiento"
        TokIn             -> "\"in\" - Parámetro de Entrada"
        TokOut            -> "\"out\" - Parámetro de Salida"
        TokInOut          -> "\"inout\" - Parámetro de Entrada y Salida"
        TokRef            -> "\"ref\" - Parámetro por Referencia"

        TokWith           -> "\"with\" - Indicador de Archivo de Entrada"

        TokVar            -> "\"var\" - Definidor de Variables"
        TokConst          -> "\"const\" - Definidor de Constantes"
        TokOf             -> "\"of\" - of del Arreglo"

        TokAsig           -> "\":=\" - Asignación"

        TokPlus           -> "\"+\" - Suma"
        TokMinus          -> "\"-\" - Resta"
        TokTimes          -> "\"*\" - Multiplicación"
        TokDiv            -> "\"/\" - División"
        TokMod            -> "\"mod\" - Modulo"
        TokPower          -> "\"^\" - Potencia"                       --"


        TokAbs            -> "\"abs\" - Valor Absoluto"
        TokSqrt           -> "\"sqrt\" - Raíz Cuadrada"

        TokComma          -> "\",\" - Coma"
        TokColon          -> "\":\" - Dos Puntos"
        TokSemicolon      -> "\";\" - Punto y Coma"
        TokArrow          -> "\"->\" - Flecha"

        TokEQ             -> "\"==\" - Equivalencia"
        TokNE             -> "\"!=\" - Inequivalencia"
        TokLE             -> "\"<=\" - Menor o Igual que"
        TokGE             -> "\">=\" - Mayor o Igual que"
        TokLT             -> "\"<\" - Menor que"
        TokGT             -> "\">\" - Mayor que"

        TokAnd            -> "\"/\\\" - Conjunción Lógica"
        TokOr             -> "\"\\/\" - Disyunción Lógica"
        TokNot            -> "\"!\" - Negación"

        TokImplies        -> "\"==>\" - Implicación"
        TokConsequent     -> "\"<==\" - Consecuencia"

        TokLeftPar        -> "\"(\" - Paréntesis Izquierdo"
        TokRightPar       -> "\")\" - Paréntesis Derecho"

        TokLeftPercent    -> "\"(%\" - Apertura de Cuantificador"
        TokRightPercent   -> "\"%)\" - Cierre de Cuantificador"

        TokLeftBracket    -> "\"[\" - Corchete Izquierdo"
        TokRightBracket   -> "\"]\" - Corchete Derecho"

        TokLeftBrace      -> "\"{\" - Llave Izquierda"
        TokRightBrace     -> "\"}\" - Llave Derecho"

        TokOpenBlock      -> "\"|[\" - Apertura de Bloque"
        TokCloseBlock     -> "\"]|\" - Cierre de Bloque"

        TokLeftPre        -> "\"{pre\" - Apertura de Precondición"
        TokRightPre       -> "\"pre}\" - Cierre de Precondición"
        TokLeftPost       -> "\"{post\" - Apertura de Postcondición"
        TokRightPost      -> "\"post}\" - Cierre de Postcondición"
        TokLeftBound      -> "\"{bound\" - Apertura de la Función de Cota"
        TokRightBound     -> "\"bound}\" - Cierre de la Función de Cota"
        TokLeftA          -> "\"{a\" - Apertura de Aserción"
        TokRightA         -> "\"a}\" - Cierre de Aserción"
        TokLeftInv        -> "\"{inv\" - Apertura de Invariante"
        TokRightInv       -> "\"inv}\" - Cierre de Invariante"
        TokPipe           -> "\"|\" - Barra Vertical"

        TokMax            -> "\"max\" - Máximo"
        TokMin            -> "\"min\" - Mínimo"
        TokForall         -> "\"forall\" - Para Todo"
        TokExist          -> "\"exist\" - Existencial"
        TokNotExist       -> "\"not-exist\" - Existencial Negado"
        TokSigma          -> "\"sigma\" - Sumatoria"
        TokPi             -> "\"pi\" - Productoria"

        TokIf             -> "\"if\" - Apertura de Selector"
        TokFi             -> "\"fi\" - Cierre de Selector"

        TokDo             -> "\"do\" - Apertura de Repetidor"
        TokOd             -> "\"od\" - Cierre de Repetidor"

        TokSepGuards      -> "\"[]\" - Separador de Guardias"

        TokAbort          -> "\"abort\" - Abortador de Programa"
        TokSkip           -> "\"skip\" - Instrucción de Salto"

        TokRandom         -> "\"random\" - Random"
        TokWrite          -> "\"write\" - Escritor"
        TokWriteln        -> "\"writeln\" - Escritor con Salto de Línea"
        TokRead           -> "\"read\" - Lector"

        TokToInt          -> "\"toInt\" - Conversión a Entero"
        TokToDouble       -> "\"toDouble\" - Conversión a Flotante"
        TokToChar         -> "\"toChar\" - Conversión a Caracter"

        TokMinInt         -> "\"MIN_INT\" - Mínimo Entero"
        TokMinDouble      -> "\"MIN_DOUBLE\" - Mínimo Flotante"
        TokMaxInt         -> "\"MAX_INT\" - Máximo Entero"
        TokMaxDouble      -> "\"MAX_DOUBLE\" - Máximo Flotante"

        (TokBool       b) -> "\"" ++ showBool b ++ "\" - Booleano"
        (TokChar       c) -> "" ++ show c ++ " - Caracter"
        (TokInteger    n) -> "\"" ++ show n ++ "\" - Entero"
        (TokFloat      n) -> " \"" ++ show n ++ "\" - Flotante"
        (TokString     e) -> "" ++ show e ++ " - Cadena de Caracteres"

        TokArray          -> "\"array\" - Tipo Arreglo"
        (TokType       t) -> "\"" ++ show t ++ "\" - Tipo " ++ showType t

        (TokId         i) -> "" ++ show i ++ " - Variable"

        TokComment        -> "\"//\" - Comentatios"
        EmptyToken        -> "Token Vacío"
        (TokUnexpected e) -> show e ++ " - Caracter no Permitido"

        -- V2.0
        TokDataType -> "type"     
        TokImplements     -> "implements"       
        TokAbstract       -> "abstract"     
        TokLeftRep        -> "{repinv"          -- UGLY
        TokRightRep       -> "repinv}"        -- UGLY
        TokLeftAcopl      -> "{acinv"            -- UGLY
        TokRightAcopl     -> "acinv}"        -- UGLY

        where
            showType :: Type -> String
            showType GInt   = "Entero "
            showType GFloat = "Flotante "
            showType GChar  = "Caracter "
            showType GBool  = "Booleano "
            showType _      = ""

            showBool :: Bool -> String
            showBool True = "true"
            showBool False = "false"
