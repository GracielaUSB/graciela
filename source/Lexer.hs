{-|
Module      : Lexer
Description : Analizador lexicografico
Copyright   : Graciela

Módulo del analizador lexicográfico,
produce una lista con los lexemas encontrados en el archivo.
-}
module Lexer (lexer) where
--------------------------------------------------------------------------------
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Monad         (void)
import           Control.Applicative   (empty)
import           Data.Functor          (($>))
import           Data.Functor.Identity (Identity)
import           Data.Text             (Text, cons, pack)
import           Text.Megaparsec       ((<|>), Dec, Parsec, ParsecT
                                       ,alphaNumChar, between, char, eof
                                       ,getPosition, letterChar, many
                                       ,notFollowedBy, parseTest, spaceChar
                                       ,string, unexpected, hidden, manyTill, try)
import qualified Text.Megaparsec.Lexer as L
--------------------------------------------------------------------------------

type Lexer = Parsec Dec Text


sc :: Lexer ()
sc = L.space (void spaceChar) lineComment blockComment
  where
    lineComment  = L.skipLineComment  "//"
    blockComment = L.skipBlockCommentNested "/*" "*/"


symbol :: String -> Lexer String
symbol = L.symbol sc


reserved :: String -> Lexer ()
reserved w =
  string w *> notFollowedBy (alphaNumChar <|> char '_' <|> char '?') *> sc


lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc


floatLit :: Lexer Token
floatLit = TokFloat <$> lexeme L.float


intLit :: Lexer Token
intLit = TokInteger <$> lexeme L.integer


charLit :: Lexer Token
charLit = TokChar <$> lexeme p
  where
    p = char '\'' *> L.charLiteral <* char '\''


stringLit :: Lexer Token
stringLit = TokString . pack <$> lexeme p
  where
    p = char '"' *> manyTill L.charLiteral (char '"')


identifier :: Lexer Token
identifier = TokId . pack <$> lexeme p
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '?')


token :: Lexer Token
token  =  (reserved "program"     *> return TokProgram)
      <|> (reserved "begin"       *> return TokBegin)
      <|> (reserved "end"         *> return TokEnd)
      <|> (reserved "func"        *> return TokFunc)
      <|> (reserved "proc"        *> return TokProc)
      <|> (reserved "inout"       *> return TokIn)
      <|> (reserved "in"          *> return TokOut)
      <|> (reserved "out"         *> return TokInOut)
      <|> (reserved "ref"         *> return TokRef)
      <|> (symbol   ":="          *> return TokAsig)
      <|> (symbol   "\8788"       *> return TokAsig) -- ≔

      <|> (symbol   ","           *> return TokComma)
      <|> (symbol   ":"           *> return TokColon)
      <|> (symbol   ";"           *> return TokSemicolon)
      <|> (symbol   "->"          *> return TokArrow)
      <|> (symbol   "\8594"       *> return TokArrow) -- →

      <|> (reserved "with"        *> return TokWith)

      -- V2.0
      <|> (reserved "type"        *> return TokDataType)
      <|> (reserved "implements"  *> return TokImplements)
      <|> (reserved "abstract"    *> return TokAbstract)
      <|> (reserved "{repinv"     *> return TokLeftRep)
      <|> (reserved "repinv}"     *> return TokRightRep)
      <|> (reserved "{coupinv"    *> return TokLeftAcopl)
      <|> (symbol   "coupinv}"    *> return TokRightAcopl)

      <|> (reserved "elem"        *> return TokElem)
      <|> (symbol   "\8712"       *> return TokElem)    -- ∈
      <|> (reserved "notelem"     *> return TokNotElem)
      <|> (symbol   "\8713"       *> return TokNotElem) -- ∉
      -- V2.0

      <|> (reserved "var"         *> return TokVar)
      <|> (reserved "const"       *> return TokConst)
      <|> (reserved "of"          *> return TokOf)
      <|> (reserved "array"       *> return TokArray)

      <|> (symbol   "/\\"         *> return TokAnd)
      <|> (symbol   "\8743"       *> return TokAnd) -- ∧
      <|> (symbol   "\\/"         *> return TokOr)
      <|> (symbol   "\8744"       *> return TokOr)  -- ∨

      -- V2.0
      <|> (reserved "set"         *> return TokSet)
      <|> (reserved "multiset"    *> return TokMultiset)
      <|> (reserved "seq"         *> return TokSeq)
      <|> (reserved "rel"         *> return TokRel)

      <|> (reserved "emptyset"    *> return TokEmptySet)
      <|> (symbol   "\8709"       *> return TokEmptySet) -- ∅
      <|> (symbol   "\\"          *> return TokSetMinus)
      <|> (reserved "union"       *> return TokSetUnion)
      <|> (symbol   "\8746"       *> return TokSetUnion) -- ∪
      <|> (reserved "intersect"   *> return TokSetIntersect)
      <|> (symbol   "\8745"       *> return TokSetUnion) -- ∩

      <|> (reserved "new"         *> return TokNew)
      <|> (reserved "free"        *> return TokFree)
      -- V2.0

      <|> (symbol   "+"           *> return TokPlus)
      <|> (symbol   "-"           *> return TokMinus)
      <|> (symbol   "*"           *> return TokTimes)
      <|> (symbol   "\215"        *> return TokTimes) -- ×
      <|> (symbol   "/"           *> return TokDiv)
      <|> (symbol   "\247"        *> return TokDiv)   -- ÷
      <|> (reserved "mod"         *> return TokMod)
      <|> (symbol   "^"           *> return TokPower)

      <|> (reserved "abs"         *> return TokAbs)
      <|> (reserved "sqrt"        *> return TokSqrt)
      <|> (symbol   "\8730"       *> return TokSqrt) -- √

      <|> (symbol   "=="          *> return TokEQ)
      <|> (symbol   "\8801"       *> return TokEQ)   -- ≡
      <|> (symbol   "!="          *> return TokNE)
      <|> (symbol   "\8800"       *> return TokNE)   -- ≠
      <|> (symbol   "\8802"       *> return TokNE)   -- ≢
      <|> (symbol   "<="          *> return TokLE)
      <|> (symbol   "\8804"       *> return TokLE)   -- ≤
      <|> (symbol   ">="          *> return TokGE)
      <|> (symbol   "\8805"       *> return TokGE)   -- ≥
      <|> (symbol   "<"           *> return TokLT)
      <|> (symbol   ">"           *> return TokGT)

      <|> (symbol   "!"           *> return TokNot)
      <|> (symbol   "\172"        *> return TokNot)  -- ¬

      <|> (symbol   "==>"         *> return TokImplies)
      <|> (symbol   "\8658"       *> return TokImplies)    -- ⇒
      <|> (symbol   "<=="         *> return TokConsequent)
      <|> (symbol   "\8656"       *> return TokConsequent) -- ⇐

      <|> (symbol   "(%"          *> return TokLeftPercent)
      <|> (symbol   "%)"          *> return TokRightPercent)

      <|> (symbol   "("           *> return TokLeftPar)
      <|> (symbol   ")"           *> return TokRightPar)

      <|> (symbol   "[]"          *> return TokSepGuards)

      <|> (symbol   "|["          *> return TokOpenBlock)
      <|> (symbol   "]|"          *> return TokCloseBlock)

      <|> (symbol   "\10214"      *> return TokOpenBlock)  -- ⟦
      <|> (symbol   "\10215"      *> return TokCloseBlock) -- ⟧

      <|> (symbol   "["           *> return TokLeftBracket)
      <|> (symbol   "]"           *> return TokRightBracket)

      <|> (reserved "{pre"        *> return TokLeftPre)
      <|> (symbol   "pre}"        *> return TokRightPre)

      <|> (reserved "{post"       *> return TokLeftPost)
      <|> (symbol   "post}"       *> return TokRightPost)

      <|> (reserved "{bound"      *> return TokLeftBound)
      <|> (symbol   "bound}"      *> return TokRightBound)

      <|> (reserved "{a"          *> return TokLeftA)
      <|> (symbol   "a}"          *> return TokRightA)

      <|> (reserved "{inv"        *> return TokLeftInv)
      <|> (symbol   "inv}"        *> return TokRightInv)

      <|> (symbol   "{"           *> return TokLeftBrace)
      <|> (symbol   "}"           *> return TokRightBrace)

      <|> (symbol   "|"           *> return TokPipe)

      <|> (reserved "max"         *> return TokMax)
      <|> (reserved "min"         *> return TokMin)
      <|> (reserved "forall"      *> return TokForall)
      <|> (symbol   "\8704"       *> return TokForall)   -- ∀
      <|> (reserved "exist"       *> return TokExist)
      <|> (symbol   "\8707"       *> return TokExist)    -- ∃
      <|> (reserved "notexist"    *> return TokNotExist)
      <|> (symbol   "\8708"       *> return TokNotExist) -- ∄
      <|> (reserved "sigma"       *> return TokSigma)
      <|> (symbol   "\8721"       *> return TokSigma)    -- ∑
      <|> (reserved "pi"          *> return TokPi)
      <|> (symbol   "\8719"       *> return TokPi)       -- ∏

      <|> (reserved "if"          *> return TokIf)
      <|> (reserved "fi"          *> return TokFi)

      <|> (reserved "do"          *> return TokDo)
      <|> (reserved "od"          *> return TokOd)

      <|> (reserved "abort"       *> return TokAbort)
      <|> (reserved "skip"        *> return TokSkip)

      <|> (reserved "random"      *> return TokRandom)
      <|> (reserved "write"       *> return TokWrite)
      <|> (reserved "writeln"     *> return TokWriteln)
      <|> (reserved "read"        *> return TokRead)

      <|> (reserved "toChar"      *> return TokToChar)
      <|> (reserved "toInt"       *> return TokToInt)
      <|> (reserved "toDouble"    *> return TokToDouble)

      <|> (reserved "MIN_INT"     *> return TokMinInt)
      <|> (reserved "MIN_DOUBLE"  *> return TokMinDouble)
      <|> (reserved "MAX_INT"     *> return TokMaxInt)
      <|> (reserved "MAX_DOUBLE"  *> return TokMaxDouble)

      <|> (reserved "true"        *> return (TokBool True))
      <|> (reserved "false"       *> return (TokBool False))

      <|> charLit
      <|> try floatLit
      <|> intLit
      <|> stringLit
      <|> identifier


tokenPos :: Lexer TokenPos
tokenPos = flip (,) <$> getPosition <*> hidden token


lexer :: Lexer [TokenPos]
lexer = between sc eof (many tokenPos)
