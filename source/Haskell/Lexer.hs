{-|
Module      : Language.Graciela.Lexer
Description : Lexical analyzer for Graciela
Copyright   : © 2015-2016 Graciela USB
Maintainer  : moises+graciela@ackerman.space
Stability   : experimental
Portability : POSIX

Lexical analysis module for Graciela,
defines all possible Graciela tokens and transforms a text into a
list of tokens.
-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Lexer
  ( lex
  ) where
--------------------------------------------------------------------------------
import           Common
import           Pragma
import           Token
--------------------------------------------------------------------------------
import           Control.Lens          (makeLenses, use, (%~))
import           Control.Monad.State   (State, evalState, modify)
import           Data.Set              (Set, union, (\\))
import qualified Data.Set              as Set (empty)
import           Data.Text             (Text, pack)
import           Prelude               hiding (lex)
import           Text.Megaparsec       (Dec, ParsecT, alphaNumChar, anyChar,
                                        between, char, eof, getPosition,
                                        letterChar, many, manyTill,
                                        notFollowedBy, oneOf, runParserT,
                                        spaceChar, string, try, (<|>))
import qualified Text.Megaparsec.Lexer as L
--------------------------------------------------------------------------------

-- | Giving the Lexer a state allows recognition of Pragmas.

data LexerState = LexerState
  { _programSeen :: Bool
  , _pragmas     :: Set Pragma }

makeLenses ''LexerState

-- | Initially, no pragmas are activated, and the Program token hasn't been
-- recognized.

initialLexerState :: LexerState
initialLexerState = LexerState
  { _programSeen = False
  , _pragmas     = Set.empty }
--------------------------------------------------------------------------------

-- | @lex filename input@ turns a 'Text' into a list of 'TokenPos'.

lex :: FilePath -- ^ Name of source file
    -> Text     -- ^ Input for parser
    -> ([TokenPos], Set Pragma)
lex fn input = case evalState (runParserT lexer fn input) initialLexerState of
  Right ts -> ts
  Left  _  -> internal "uncaught unexpected token"
--------------------------------------------------------------------------------

type Lexer = ParsecT Dec Text (State LexerState)


lexer :: Lexer ([TokenPos], Set Pragma)
lexer = (,) <$> between sc eof (many token) <*> use pragmas


sc :: Lexer ()
sc = L.space (void spaceChar) lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment =  try (string "/*%" >> pragma)
                <|> L.skipBlockCommentNested "/*" "*/"
    pragma = do
      seen <- use programSeen
      unless seen $ do
        many (void spaceChar)
        string "LANGUAGE"
        many (void spaceChar)
        p <- pragma'
        many (void spaceChar)
        void $ string "%*/"
        modify p
    pragma' = (pragmas %~) <$> pragma''
    pragma'' =  (string "LogicAnywhere"   $> union [LogicAnywhere])
            <|> (string "NoLogicAnywhere" $> (\\)  [LogicAnywhere])
            <|> (string "EnableTrace"     $> union [EnableTrace])
            <|> (string "NoEnableTrace"   $> (\\)  [EnableTrace])


lexeme :: Lexer Token -> Lexer TokenPos
lexeme p = flip . TokenPos <$> getPosition <*> p <*> getPosition <* sc


symbol :: String -> Token -> Lexer TokenPos
symbol w tok = try . lexeme $
  string w *>
  pure tok

reserved :: String -> Token -> Lexer TokenPos
reserved w tok = try . lexeme $
  string w *>
  notFollowedBy (alphaNumChar <|> char '_' <|> char '?' <|> char '\'') *>
  pure tok


charLit :: Lexer TokenPos
charLit = lexeme $
  TokChar <$> (char '\'' *> L.charLiteral <* char '\'')


intLit :: Lexer TokenPos
intLit = lexeme $ do
  n <- L.integer
  pure $ if n > fromIntegral (maxBound :: Int32)
    then TokBadInteger n
    else TokInteger . fromInteger $ n


floatLit :: Lexer TokenPos
floatLit = lexeme $
  TokFloat <$> L.float


stringLit :: Lexer TokenPos
stringLit = lexeme $
  TokString . pack <$> (char '"' *> manyTill L.charLiteral (char '"'))


identifier :: Lexer TokenPos
identifier = lexeme $
  TokId . pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> oneOf "_?'"))


unexpected :: Lexer TokenPos
unexpected = lexeme $ TokUnexpected <$> anyChar


token :: Lexer TokenPos
token  =  reserved "program"    TokProgram
      <|> reserved "main"       TokMain
      <|> reserved "begin"      TokBegin
      <|> reserved "end"        TokEnd
      <|> reserved "func"       TokFunc
      <|> reserved "proc"       TokProc
      <|> reserved "in"         TokIn
      <|> reserved "out"        TokOut
      <|> reserved "inout"      TokInOut
      <|> reserved "ref"        TokRef
      <|> symbol   ":="         TokAssign
      <|> symbol   "\8788"      TokAssign -- ≔

      -- 2.0
      <|> symbol   "{:"         TokLeftBag
      <|> symbol   ":}"         TokRightBag
      -- 2.0

      <|> symbol   "."          TokDot
      <|> symbol   ","          TokComma
      <|> symbol   ":"          TokColon
      <|> symbol   ";"          TokSemicolon
      <|> symbol   "->"         TokArrow
      <|> symbol   "\8594"      TokArrow -- →
      <|> symbol   "<->"        TokBiArrow
      <|> symbol   "\8596"      TokBiArrow -- ↔

      <|> reserved "from"       TokFrom

      -- V2.0
      <|> reserved "type"       TokType
      <|> reserved "implements" TokImplements
      <|> reserved "abstract"   TokAbstract
      <|> reserved "{repinv"    TokLeftRep
      <|> reserved "repinv}"    TokRightRep
      <|> reserved "{coupinv"   TokLeftAcopl
      <|> symbol   "coupinv}"   TokRightAcopl

      <|> reserved "elem"       TokElem
      <|> symbol   "\8712"      TokElem    -- ∈
      <|> reserved "notelem"    TokNotElem
      <|> symbol   "\8713"      TokNotElem -- ∉
      -- V2.0

      <|> reserved "var"        TokVar
      <|> reserved "const"      TokConst
      <|> reserved "of"         TokOf
      <|> reserved "array"      TokArray

      <|> symbol   "/\\"        TokAnd
      <|> symbol   "\8743"      TokAnd -- ∧
      <|> symbol   "\\/"        TokOr
      <|> symbol   "\8744"      TokOr  -- ∨

      -- V2.0
      <|> reserved "set"        TokSet
      <|> reserved "multiset"   TokMultiset
      <|> reserved "sequence"   TokSequence
      -- <|> reserved "rel"        TokRel
      <|> reserved "function"   TokFunction
      <|> reserved "relation"   TokRelation

      <|> symbol   "\\"         TokSetMinus
      <|> reserved "union"      TokSetUnion
      <|> symbol   "\8746"      TokSetUnion -- ∪
      <|> reserved "intersect"  TokSetIntersect
      <|> symbol   "\8745"      TokSetUnion -- ∩

      <|> reserved "msum"       TokMultisetSum
      <|> symbol   "\8846"      TokMultisetSum -- ⊎
      <|> symbol   "++"         TokConcat
      <|> symbol   "\10746"     TokConcat -- ⧺
      <|> reserved "subset"     TokSubset
      <|> symbol   "\8838"      TokSubset    -- ⊆
      <|> reserved "ssubset"    TokSSubset
      <|> symbol   "\8834"      TokSSubset   -- ⊂
      <|> symbol   "\8842"      TokSSubset   -- ⊊
      <|> reserved "superset"   TokSuperset
      <|> symbol   "\8839"      TokSuperset  -- ⊇
      <|> reserved "ssuperset"  TokSSuperset
      <|> symbol   "\8835"      TokSSuperset -- ⊃
      <|> symbol   "\8843"      TokSSuperset -- ⊋

      <|> reserved "new"        TokNew
      <|> reserved "free"       TokFree
      -- V2.0

      <|> symbol   "+"          TokPlus
      <|> symbol   "-"          TokMinus
      <|> symbol   "*"          TokTimes
      <|> symbol   "\215"       TokTimes -- ×
      <|> symbol   "/"          TokDiv
      <|> symbol   "\247"       TokDiv   -- ÷
      <|> reserved "mod"        TokMod
      <|> symbol   "^"          TokPower

      <|> symbol   "\8730"      (TokId $ pack "sqrt")  -- √

      <|> symbol   "==>"        TokImplies
      <|> symbol   "\8658"      TokImplies    -- ⇒
      <|> symbol   "<=="        TokConsequent
      <|> symbol   "\8656"      TokConsequent -- ⇐

      <|> symbol   "==="        TokBEQ
      <|> symbol   "\8801"      TokBEQ   -- ≡
      <|> symbol   "!=="        TokBNE
      <|> symbol   "\8802"      TokBNE   -- ≢

      -- V2.0
      <|> symbol   "<<"         TokLeftSeq
      <|> symbol   ">>"         TokRightSeq
      <|> symbol   "\10216"     TokLeftSeq  -- ⟨
      <|> symbol   "\10217"     TokRightSeq -- ⟩
      -- V2.0

      <|> symbol   "=="         TokAEQ
      <|> symbol   "!="         TokANE
      <|> symbol   "\8800"      TokANE   -- ≠

      <|> symbol   "="          TokBadEQ

      <|> symbol   "<="         TokLE
      <|> symbol   "\8804"      TokLE   -- ≤
      <|> symbol   ">="         TokGE
      <|> symbol   "\8805"      TokGE   -- ≥
      <|> symbol   "<"          TokLT
      <|> symbol   ">"          TokGT

      <|> symbol   "!"          TokNot
      <|> symbol   "\172"       TokNot  -- ¬

      <|> symbol   "(%"         TokLeftPercent
      <|> symbol   "%)"         TokRightPercent

      <|> symbol   "("          TokLeftPar
      <|> symbol   ")"          TokRightPar

      <|> symbol   "[]"         TokSepGuards

      <|> symbol   "|["         TokOpenBlock
      <|> symbol   "]|"         TokCloseBlock

      <|> symbol   "\10214"     TokOpenBlock  -- ⟦
      <|> symbol   "\10215"     TokCloseBlock -- ⟧

      <|> symbol   "["          TokLeftBracket
      <|> symbol   "]"          TokRightBracket

      <|> reserved "{pre"       TokLeftPre
      <|> symbol   "pre}"       TokRightPre

      <|> reserved "{post"      TokLeftPost
      <|> symbol   "post}"      TokRightPost

      <|> reserved "{bound"     TokLeftBound
      <|> symbol   "bound}"     TokRightBound

      <|> reserved "{a"         TokLeftA
      <|> symbol   "a}"         TokRightA

      <|> reserved "{inv"       TokLeftInv
      <|> symbol   "inv}"       TokRightInv


      -- V2.0
      <|> symbol   "\10181"     TokLeftBag  -- ⟅
      <|> symbol   "\10182"     TokRightBag -- ⟆

      <|> symbol   "\8709"      TokEmptySet -- ∅
      <|> symbol   "{"          TokLeftBrace
      <|> symbol   "}"          TokRightBrace
      -- V2.0

      <|> symbol   "|"          TokPipe

      <|> reserved "max"        TokMax
      <|> reserved "min"        TokMin
      <|> reserved "forall"     TokForall
      <|> symbol   "\8704"      TokForall   -- ∀
      <|> reserved "exist"      TokExist
      <|> symbol   "\8707"      TokExist    -- ∃
      <|> reserved "notexist"   TokNotExist
      <|> symbol   "\8708"      TokNotExist -- ∄
      <|> reserved "sum"        TokSum
      <|> symbol   "\8721"      TokSum      -- ∑
      <|> reserved "product"    TokProduct
      <|> symbol   "\8719"      TokProduct  -- ∏
      <|> reserved "count"      TokCount
      <|> symbol   "#"          TokHash     -- count quant and cardinality operator

      <|> reserved "if"         TokIf
      <|> reserved "fi"         TokFi

      <|> reserved "do"         TokDo
      <|> reserved "od"         TokOd

      <|> reserved "abort"      TokAbort
      <|> reserved "warn"       TokWarn
      <|> reserved "skip"       TokSkip

      <|> reserved "random"     TokRandom
      <|> reserved "write"      TokWrite
      <|> reserved "writeln"    TokWriteln
      <|> reserved "read"       TokRead

      <|> reserved "true"       (TokBool True)
      <|> reserved "false"      (TokBool False)

      <|> reserved "null"       TokNull
      <|> reserved "where"      TokWhere

      <|> charLit
      <|> try floatLit
      <|> intLit
      <|> stringLit
      <|> identifier

      <|> unexpected
