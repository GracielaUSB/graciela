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

{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Lexer
  ( lex
  ) where
--------------------------------------------------------------------------------
import           Location
import           Token
--------------------------------------------------------------------------------
import           Control.Monad         (void)
import           Data.Functor          (($>))
import           Data.Int              (Int32)
import           Data.Monoid           ((<>))
import           Data.Text             (Text, pack)
import           Prelude               hiding (lex)
import           Text.Megaparsec       (Dec, ParseError, Parsec, alphaNumChar,
                                        anyChar, between, char, eof,
                                        getPosition, hidden, letterChar, many,
                                        manyTill, notFollowedBy, oneOf,
                                        optional, runParser, spaceChar, string,
                                        try, (<|>))
import qualified Text.Megaparsec.Lexer as L
--------------------------------------------------------------------------------

-- | @lex filename input@ turns a 'Text' into a list of 'TokenPos'.

lex :: FilePath -- ^ Name of source file
    -> Text     -- ^ Input for parser
    -> [TokenPos]
lex fn input = case runParser lexer fn input of
  Right ts -> ts
  Left  _  -> error "internal error: uncaught unexpected token"
--------------------------------------------------------------------------------

type Lexer = Parsec Dec Text


lexer :: Lexer [TokenPos]
lexer = between sc eof (many token)


sc :: Lexer ()
sc = L.space (void spaceChar) lineComment blockComment
  where
    lineComment  = L.skipLineComment  "//"
    blockComment = L.skipBlockCommentNested "/*" "*/"


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


emptyMultiset :: Lexer TokenPos
emptyMultiset = try . lexeme $ ascii <|> utf8
  where
    ascii =
      symbol "{{"     TokLeftBag  *>
      symbol "}}"     TokRightBag *>
      pure TokEmptyMultiset
    utf8 =
      symbol "\10181" TokLeftBag  *>
      symbol "\10182" TokRightBag *>
      pure TokEmptyMultiset


emptySet :: Lexer TokenPos
emptySet = try . lexeme $
  symbol "{" TokLeftBrace  *>
  symbol "}" TokRightBrace *>
  pure TokEmptySet


unexpected :: Lexer TokenPos
unexpected = lexeme $ TokUnexpected <$> anyChar


token :: Lexer TokenPos
token  =  reserved "program"    TokProgram
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
      <|> reserved "seq"        TokSeq
      <|> reserved "rel"        TokRel

      <|> symbol   "\\"         TokSetMinus
      <|> reserved "union"      TokSetUnion
      <|> symbol   "\8746"      TokSetUnion -- ∪
      <|> reserved "intersect"  TokSetIntersect
      <|> symbol   "\8745"      TokSetUnion -- ∩

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

      <|> reserved "abs"        TokAbs
      <|> reserved "sqrt"       TokSqrt
      <|> symbol   "\8730"      TokSqrt -- √

      <|> symbol   "==>"        TokImplies
      <|> symbol   "\8658"      TokImplies    -- ⇒
      <|> symbol   "<=="        TokConsequent
      <|> symbol   "\8656"      TokConsequent -- ⇐

      <|> symbol   "==="        TokBEQ
      <|> symbol   "\8801"      TokBEQ   -- ≡
      <|> symbol   "!=="        TokBNE
      <|> symbol   "\8802"      TokBNE   -- ≢

      <|> symbol   "==>"        TokImplies
      <|> symbol   "\8658"      TokImplies    -- ⇒
      <|> symbol   "<=="        TokConsequent
      <|> symbol   "\8656"      TokConsequent -- ⇐

      <|> symbol   "=="         TokAEQ
      <|> symbol   "!="         TokANE
      <|> symbol   "\8800"      TokANE   -- ≠

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
      <|> emptyMultiset                     -- ⟅ ⟆ or {{ }}
      <|> symbol   "{{"         TokLeftBag
      <|> symbol   "\10181"     TokLeftBag  -- ⟅
      <|> symbol   "}}"         TokRightBag
      <|> symbol   "\10182"     TokRightBag -- ⟆

      <|> emptySet                          -- { }
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
      <|> reserved "sigma"      TokSigma
      <|> symbol   "\8721"      TokSigma    -- ∑
      <|> reserved "pi"         TokPi
      <|> symbol   "\8719"      TokPi       -- ∏
      <|> reserved "count"      TokCount
      <|> symbol   "#"          TokCount

      <|> reserved "if"         TokIf
      <|> reserved "fi"         TokFi

      <|> reserved "do"         TokDo
      <|> reserved "od"         TokOd

      <|> reserved "abort"      TokAbort
      <|> reserved "skip"       TokSkip

      <|> reserved "random"     TokRandom
      <|> reserved "write"      TokWrite
      <|> reserved "writeln"    TokWriteln
      <|> reserved "read"       TokRead

      <|> reserved "toChar"     TokToChar
      <|> reserved "toInt"      TokToInt
      <|> reserved "toDouble"   TokToDouble

      <|> reserved "true"       (TokBool True)
      <|> reserved "false"      (TokBool False)

      <|> reserved "null"       TokNull

      <|> charLit
      <|> try floatLit
      <|> intLit
      <|> stringLit
      <|> identifier

      <|> unexpected
