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
  )where
--------------------------------------------------------------------------------
import           Location
--------------------------------------------------------------------------------
import           Data.Function      (on)
import           Data.Int           (Int32)
import           Data.List.NonEmpty (toList)
import           Data.Semigroup     ((<>))
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
  showTokens = unwords . fmap show . toList


-- Tokens ------------------------------
-- | Representa a todos las palabras reservadas en el lenguaje
data Token
  = TokProgram
  | TokMain
  | TokBegin
  | TokEnd

  | TokFunc
  | TokProc
  | TokIn
  | TokOut
  | TokInOut
  | TokRef

  | TokFrom

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

  | TokLeftSeq
  | TokRightSeq

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
  | TokHash

  | TokIf
  | TokFi

  | TokDo
  | TokOd

  | TokSepGuards

  | TokAbort
  | TokWarn
  | TokSkip

  | TokRandom
  | TokWrite
  | TokWriteln
  | TokRead

  -- | TokToInt
  -- | TokToDouble
  -- | TokToChar

  -- | TokMinInt
  -- | TokMinDouble
  -- | TokMaxInt
  -- | TokMaxDouble

  | TokBool       { unTokBool :: Bool }
  | TokChar       { unTokChar :: Char }
  | TokInteger    { unTokInteger :: Int32 }
  | TokBadInteger { unTokBadInteger :: Integer }
  | TokFloat      { unTokFloat :: Double}
  | TokString     { unTokString :: Text }

  | TokArray
  | TokId         { unTokId :: Text }

  -- | TokComment
  -- | EmptyToken
  | TokUnexpected { unTokUnexpected :: Char}

  -- V2.0
  | TokType
  | TokImplements
  | TokAbstract
  | TokLeftRep
  | TokRightRep
  -- | TokLeftAcopl
  -- | TokRightAcopl

  | TokElem
  | TokNotElem
  | TokSetMinus
  | TokSetUnion
  | TokSetIntersect

  | TokMultisetSum
  | TokAtSign
  | TokConcat
  | TokSubset
  | TokSSubset
  | TokSuperset
  | TokSSuperset

  | TokEmptySet

  | TokSet
  | TokMultiset
  | TokSeq
  | TokRel

  | TokNew
  | TokFree
  | TokNull
  | TokWhere

  deriving (Eq, Ord)


-- | Instancia 'Show' para los tokens
instance Show Token where
  show = \case
    TokProgram        -> "`program`"
    TokMain           -> "`main`"
    TokBegin          -> "`begin`"
    TokEnd            -> "`end`"

    TokFunc           -> "`func`"
    TokProc           -> "`proc`"
    TokIn             -> "`in`"
    TokOut            -> "`out`"
    TokInOut          -> "`inout`"
    TokRef            -> "`ref`"

    TokFrom           -> "`with`"

    TokVar            -> "`var`"
    TokConst          -> "`const`"
    TokOf             -> "`of`"

    TokAssign           -> "`:=`"

    TokPlus           -> "`+`"
    TokMinus          -> "`-`"
    TokTimes          -> "`*`"
    TokDiv            -> "`/`"
    TokMod            -> "`mod`"
    TokPower          -> "`^`"

    TokDot            -> "`.`"
    TokComma          -> "`,`"
    TokColon          -> "`:`"
    TokSemicolon      -> "`;`"
    TokArrow          -> "`->`"
    TokBiArrow        -> "`<->`"

    TokBEQ            -> "`===`"
    TokBNE            -> "`!==`"

    TokAEQ            -> "`==`"
    TokANE            -> "`!=`"

    TokLE             -> "`<=`"
    TokGE             -> "`>=`"
    TokLT             -> "`<`"
    TokGT             -> "`>`"

    TokAnd            -> "`/\\`"
    TokOr             -> "`\\/`"
    TokNot            -> "`!`"

    TokImplies        -> "`==>`"
    TokConsequent     -> "`<==`"

    TokLeftPar        -> "`(`"
    TokRightPar       -> "`)`"

    TokLeftPercent    -> "`(%`"
    TokRightPercent   -> "`%)`"

    TokLeftBracket    -> "`[`"
    TokRightBracket   -> "`]`"

    TokLeftBrace      -> "`{`"
    TokRightBrace     -> "`}`"

    TokLeftBag        -> "`{{`"
    TokRightBag       -> "`}}`"

    TokLeftSeq        -> "`<<`"
    TokRightSeq       -> "`>>`"

    TokOpenBlock      -> "`|[`"
    TokCloseBlock     -> "`]|`"

    TokLeftPre        -> "`{pre`"
    TokRightPre       -> "`pre}`"
    TokLeftPost       -> "`{post`"
    TokRightPost      -> "`post}`"
    TokLeftBound      -> "`{bound`"
    TokRightBound     -> "`bound}`"
    TokLeftA          -> "`{a`"
    TokRightA         -> "`a}`"
    TokLeftInv        -> "`{inv`"
    TokRightInv       -> "`inv}`"
    TokPipe           -> "`|`"

    TokMax            -> "`max`"
    TokMin            -> "`min`"
    TokForall         -> "`forall`"
    TokExist          -> "`exist`"
    TokNotExist       -> "`not-exist`"
    TokSigma          -> "`sigma`"
    TokPi             -> "`pi`"
    TokCount          -> "`count`"
    TokHash           -> "`#`"

    TokIf             -> "`if`"
    TokFi             -> "`fi`"

    TokDo             -> "`do`"
    TokOd             -> "`od`"

    TokSepGuards      -> "`[]`"

    TokAbort          -> "`abort`"
    TokWarn           -> "`warn`"
    TokSkip           -> "`skip`"

    TokRandom         -> "`random`"
    TokWrite          -> "`write`"
    TokWriteln        -> "`writeln`"
    TokRead           -> "`read`"

    -- TokToInt          -> "`toInt` - Conversión a Entero"
    -- TokToDouble       -> "`toDouble` - Conversión a Flotante"
    -- TokToChar         -> "`toChar` - Conversión a Caracter"

    -- TokMinInt         -> "`MIN_INT` - Mínimo Entero"
    -- TokMinDouble      -> "`MIN_DOUBLE` - Mínimo Flotante"
    -- TokMaxInt         -> "`MAX_INT` - Máximo Entero"
    -- TokMaxDouble      -> "`MAX_DOUBLE` - Máximo Flotante"

    (TokBool    True) -> "`true`"
    (TokBool   False) -> "`false`"
    (TokChar       c) -> "" <> show c <> ""
    (TokInteger    n) -> "`" <> show n <> "`"
    (TokBadInteger n) -> "`" <> show n <> "`"
    (TokFloat      n) -> " `" <> show n <> "`"
    (TokString     s) -> "" <> show s <> ""

    TokArray          -> "`array`"

    (TokId         i) -> "\"" <> unpack i <> "\""

    -- TokComment        -> "`//` - Comentatios"
    -- EmptyToken        -> "Token Vacío"
    -- (TokUnexpected e) -> show e <> " - Caracter no Permitido"

    -- V2.0
    TokType           -> "`type`"
    TokImplements     -> "`implements`"
    TokAbstract       -> "`abstract`"
    TokLeftRep        -> "`{repinv`"       -- UGLY
    TokRightRep       -> "`repinv`}"       -- UGLY
    -- TokLeftAcopl      -> "`{coupinv`"      -- UGLY
    -- TokRightAcopl     -> "`coupinv`}"      -- UGLY

    TokElem           -> "`elem`"
    TokNotElem        -> "`notelem`"
    TokSetMinus       -> "`\\`"
    TokSetUnion       -> "`union`"
    TokSetIntersect   -> "`intersect`"

    TokMultisetSum    -> "`msum`"
    TokAtSign         -> "`@`"
    TokConcat         -> "`++`"
    TokSubset         -> "`subset`"
    TokSSubset        -> "`ssubset`"
    TokSuperset       -> "`superset`"
    TokSSuperset      -> "`ssuperset`"

    TokEmptySet       -> "`∅`"

    TokSet            -> "`set`"
    TokMultiset       -> "`multiset`"
    TokSeq            -> "`seq`"
    TokRel            -> "`rel`"

    TokNew            -> "`new`"
    TokFree           -> "`free`"
    TokNull           -> "`null`"
    TokWhere          -> "where"
    TokUnexpected t   -> show t
