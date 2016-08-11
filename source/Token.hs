{-|
Module      : Token
Description : Todos los lexemas del lenguaje
Copyright   : Graciela

Contiene lo referente a los lexemas (tokens) generados por el analizador
lexicográfico (lexer) para ser utilizados por el analizador semántico (parser)
-}

{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Token
  ( Token (..)
  , TokenPos (..)
  , showPos
  , showPos'
  )where
--------------------------------------------------------------------------------
import           Location
import           Type
--------------------------------------------------------------------------------
import           Data.Function      (on)
import           Data.List.NonEmpty (toList)
import           Data.Monoid        ((<>))
import           Data.Text          (Text, unpack)
import           Text.Megaparsec    (ShowToken (..))
--------------------------------------------------------------------------------
-- TokenPos ----------------------------

-- | Representa un lexema junto con su posición de inicio y fin
data TokenPos = TokenPos
  { start :: SourcePos
  , end   :: SourcePos
  , tok   :: Token
  } deriving (Eq)

instance Show TokenPos where
  show TokenPos { tok } = show tok

instance Ord TokenPos where
  compare = compare `on` start

instance ShowToken TokenPos where
  showTokens = unwords . map show . toList


-- Tokens ------------------------------
-- | Representa a todos las palabras reservadas en el lenguaje
data Token
  = TokProgram
  | TokBegin
  | TokEnd

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

  | TokAssign

  | TokPlus
  | TokMinus
  | TokTimes
  | TokDiv
  | TokMod
  | TokPower

  | TokAbs
  | TokSqrt

  | TokDot
  | TokComma
  | TokColon
  | TokSemicolon
  | TokArrow
  | TokBiArrow

  | TokBEQ -- Boolean Equivalent
  | TokBNE -- Boolean Not Equivalent

  | TokAEQ -- Arithmetic Equal
  | TokANE -- Arithmetic Not Equal

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

  | TokLeftBag
  | TokRightBag

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
  | TokCount

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

  -- | TokMinInt
  -- | TokMinDouble
  -- | TokMaxInt
  -- | TokMaxDouble

  | TokBool       { unTokBool :: Bool }
  | TokChar       { unTokChar :: Char }
  | TokInteger    { unTokInteger :: Integer }
  | TokFloat      { unTokFloatante :: Double}
  | TokString     { unTokString :: Text }

  | TokArray
  | TokId         { unTokId :: Text }
  | TokId'        { unTokId' :: Text }

  -- | TokComment
  -- | EmptyToken
  | TokUnexpected { unTokUnexpected :: Char}

  -- V2.0
  | TokType
  | TokImplements
  | TokAbstract
  | TokLeftRep
  | TokRightRep
  | TokLeftAcopl
  | TokRightAcopl

  | TokElem
  | TokNotElem
  | TokSetMinus
  | TokSetUnion
  | TokSetIntersect
  | TokEmptySet
  | TokEmptyMultiset

  | TokSet
  | TokMultiset
  | TokSeq
  | TokRel

  | TokNew
  | TokFree
  deriving (Eq, Ord)


-- | Instancia 'Show' para los tokens
instance Show Token where
  show = \case
    TokProgram        -> "`program` - Inicio del Programa"
    TokBegin          -> "`begin` - Inicio de Procedimiento o Función"
    TokEnd            -> "`end` - Fin de Procedimiento o Función"

    TokFunc           -> "`func` - Función"
    TokProc           -> "`proc` - Procedimiento"
    TokIn             -> "`in` - Parámetro de Entrada"
    TokOut            -> "`out` - Parámetro de Salida"
    TokInOut          -> "`inout` - Parámetro de Entrada y Salida"
    TokRef            -> "`ref` - Parámetro por Referencia"

    TokWith           -> "`with` - Indicador de Archivo de Entrada"

    TokVar            -> "`var` - Definidor de Variables"
    TokConst          -> "`const` - Definidor de Constantes"
    TokOf             -> "`of` - of del Arreglo"

    TokAssign           -> "`:=` - Asignación"

    TokPlus           -> "`+` - Suma"
    TokMinus          -> "`-` - Resta"
    TokTimes          -> "`*` - Multiplicación"
    TokDiv            -> "`/` - División"
    TokMod            -> "`mod` - Modulo"
    TokPower          -> "`^` - Potencia"

    TokAbs            -> "`abs` - Valor Absoluto"
    TokSqrt           -> "`sqrt` - Raíz Cuadrada"

    TokDot            -> "`.` - Punto"
    TokComma          -> "`,` - Coma"
    TokColon          -> "`:` - Dos Puntos"
    TokSemicolon      -> "`;` - Punto y Coma"
    TokArrow          -> "`->` - Flecha"
    TokBiArrow        -> "`<->` - Doble flecha"

    TokBEQ            -> "`===` - Equivalencia lógica"
    TokBNE            -> "`!==` - Inequivalencia lógica"

    TokAEQ            -> "`==` - Igualdad aritmética"
    TokANE            -> "`!=` - Desigualdad aritmética"

    TokLE             -> "`<=` - Menor o Igual que"
    TokGE             -> "`>=` - Mayor o Igual que"
    TokLT             -> "`<` - Menor que"
    TokGT             -> "`>` - Mayor que"

    TokAnd            -> "`/\\` - Conjunción Lógica"
    TokOr             -> "`\\/` - Disyunción Lógica"
    TokNot            -> "`!` - Negación"

    TokImplies        -> "`==>` - Implicación"
    TokConsequent     -> "`<==` - Consecuencia"

    TokLeftPar        -> "`(` - Paréntesis Izquierdo"
    TokRightPar       -> "`)` - Paréntesis Derecho"

    TokLeftPercent    -> "`(%` - Apertura de Cuantificador"
    TokRightPercent   -> "`%)` - Cierre de Cuantificador"

    TokLeftBracket    -> "`[` - Corchete Izquierdo"
    TokRightBracket   -> "`]` - Corchete Derecho"

    TokLeftBrace      -> "`{` - Llave Izquierda"
    TokRightBrace     -> "`}` - Llave Derecho"

    TokLeftBag        -> "`{{` - Signo de multiconjunto Izquierdo"
    TokRightBag       -> "`}}` - Signo de multiconjunto Derecho"

    TokOpenBlock      -> "`|[` - Apertura de Bloque"
    TokCloseBlock     -> "`]|` - Cierre de Bloque"

    TokLeftPre        -> "`{pre` - Apertura de Precondición"
    TokRightPre       -> "`pre}` - Cierre de Precondición"
    TokLeftPost       -> "`{post` - Apertura de Postcondición"
    TokRightPost      -> "`post}` - Cierre de Postcondición"
    TokLeftBound      -> "`{bound` - Apertura de la Función de Cota"
    TokRightBound     -> "`bound}` - Cierre de la Función de Cota"
    TokLeftA          -> "`{a` - Apertura de Aserción"
    TokRightA         -> "`a}` - Cierre de Aserción"
    TokLeftInv        -> "`{inv` - Apertura de Invariante"
    TokRightInv       -> "`inv}` - Cierre de Invariante"
    TokPipe           -> "`|` - Barra Vertical"

    TokMax            -> "`max` - Máximo"
    TokMin            -> "`min` - Mínimo"
    TokForall         -> "`forall` - Para Todo"
    TokExist          -> "`exist` - Existencial"
    TokNotExist       -> "`not-exist` - Existencial Negado"
    TokSigma          -> "`sigma` - Sumatoria"
    TokPi             -> "`pi` - Productoria"
    TokCount          -> "`count` - Cuenta"

    TokIf             -> "`if` - Apertura de Selector"
    TokFi             -> "`fi` - Cierre de Selector"

    TokDo             -> "`do` - Apertura de Repetidor"
    TokOd             -> "`od` - Cierre de Repetidor"

    TokSepGuards      -> "`[]` - Separador de Guardias"

    TokAbort          -> "`abort` - Abortador de Programa"
    TokSkip           -> "`skip` - Instrucción de Salto"

    TokRandom         -> "`random` - Random"
    TokWrite          -> "`write` - Escritor"
    TokWriteln        -> "`writeln` - Escritor con Salto de Línea"
    TokRead           -> "`read` - Lector"

    TokToInt          -> "`toInt` - Conversión a Entero"
    TokToDouble       -> "`toDouble` - Conversión a Flotante"
    TokToChar         -> "`toChar` - Conversión a Caracter"

    -- TokMinInt         -> "`MIN_INT` - Mínimo Entero"
    -- TokMinDouble      -> "`MIN_DOUBLE` - Mínimo Flotante"
    -- TokMaxInt         -> "`MAX_INT` - Máximo Entero"
    -- TokMaxDouble      -> "`MAX_DOUBLE` - Máximo Flotante"

    (TokBool    True) -> "`true` - Booleano"
    (TokBool   False) -> "`false` - Booleano"
    (TokChar       c) -> "" <> show c <> " - Caracter"
    (TokInteger    n) -> "`" <> show n <> "` - Entero"
    (TokFloat      n) -> " `" <> show n <> "` - Flotante"
    (TokString     e) -> "" <> show e <> " - Cadena de Caracteres"

    TokArray          -> "`array` - Tipo Arreglo"

    (TokId         i) -> "\"" <> unpack i <> "\" - Identificador"
    (TokId'        i) -> "\"" <> unpack i <> "\'\" - Identificador'"

    -- TokComment        -> "`//` - Comentatios"
    -- EmptyToken        -> "Token Vacío"
    -- (TokUnexpected e) -> show e <> " - Caracter no Permitido"

    -- V2.0
    TokType           -> "`type`"
    TokImplements     -> "`implements`"
    TokAbstract       -> "`abstract`"
    TokLeftRep        -> "`{repinv`"       -- UGLY
    TokRightRep       -> "`repinv`}"       -- UGLY
    TokLeftAcopl      -> "`{coupinv`"      -- UGLY
    TokRightAcopl     -> "`coupinv`}"      -- UGLY

    TokElem           -> "`elem` - Elemento de conjunto"
    TokNotElem        -> "`notelem` - Elemento de conjunto, negado"
    TokSetMinus       -> "`\\` - Resta de conjuntos"
    TokSetUnion       -> "`union` - Unión de conjuntos"
    TokSetIntersect   -> "`intersect` - Intersección de conjuntos"
    TokEmptySet       -> "`{}` - Conjunto vacío"
    TokEmptyMultiset  -> "`{{}}` - Multiconjunto vacío"

    TokSet            -> "`set` - Conjunto"
    TokMultiset       -> "`multiset` - Multiconjunto"
    TokSeq            -> "`seq` - Secuencia"
    TokRel            -> "`rel` - Relación"

    TokNew            -> "`new` - asignar una ubicación en el heap"
    TokFree           -> "`free` - liberar ubicación en el heap"
    TokUnexpected t   -> show t
