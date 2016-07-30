{-|
Module      : Lexer
Description : Analizador lexicografico
Copyright   : Graciela

Módulo del analizador lexicográfico,
produce una lista con los lexemas encontrados en el archivo.
-}
module Lexer
  ( lexer
  ) where
--------------------------------------------------------------------------------
import           Token
--------------------------------------------------------------------------------
import           Control.Monad         (void)
import           Data.Functor          (($>))
import           Data.Text             (Text, pack)
import           Text.Megaparsec       (Dec, ParseError, Parsec, alphaNumChar,
                                        between, char, eof, getPosition, hidden,
                                        letterChar, many, manyTill,
                                        notFollowedBy, runParser, spaceChar,
                                        string, try, (<|>))
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
reserved w = try $
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
token  =  (reserved "program"     $> TokProgram)
      <|> (reserved "begin"       $> TokBegin)
      <|> (reserved "end"         $> TokEnd)
      <|> (reserved "func"        $> TokFunc)
      <|> (reserved "proc"        $> TokProc)
      <|> (reserved "in"          $> TokIn)
      <|> (reserved "out"         $> TokOut)
      <|> (reserved "inout"       $> TokInOut)
      <|> (reserved "ref"         $> TokRef)
      <|> (symbol   ":="          $> TokAsig)
      <|> (symbol   "\8788"       $> TokAsig) -- ≔

      <|> (symbol   ","           $> TokComma)
      <|> (symbol   ":"           $> TokColon)
      <|> (symbol   ";"           $> TokSemicolon)
      <|> (symbol   "->"          $> TokArrow)
      <|> (symbol   "\8594"       $> TokArrow) -- →
      <|> (symbol   "<->"         $> TokBiArrow)
      <|> (symbol   "\8596"       $> TokBiArrow) -- ↔

      <|> (reserved "with"        $> TokWith)

      -- V2.0
      <|> (reserved "type"        $> TokType)
      <|> (reserved "implements"  $> TokImplements)
      <|> (reserved "abstract"    $> TokAbstract)
      <|> (reserved "{repinv"     $> TokLeftRep)
      <|> (reserved "repinv}"     $> TokRightRep)
      <|> (reserved "{coupinv"    $> TokLeftAcopl)
      <|> (symbol   "coupinv}"    $> TokRightAcopl)

      <|> (reserved "elem"        $> TokElem)
      <|> (symbol   "\8712"       $> TokElem)    -- ∈
      <|> (reserved "notelem"     $> TokNotElem)
      <|> (symbol   "\8713"       $> TokNotElem) -- ∉
      -- V2.0

      <|> (reserved "var"         $> TokVar)
      <|> (reserved "const"       $> TokConst)
      <|> (reserved "of"          $> TokOf)
      <|> (reserved "array"       $> TokArray)

      <|> (symbol   "/\\"         $> TokAnd)
      <|> (symbol   "\8743"       $> TokAnd) -- ∧
      <|> (symbol   "\\/"         $> TokOr)
      <|> (symbol   "\8744"       $> TokOr)  -- ∨

      -- V2.0
      <|> (reserved "set"         $> TokSet)
      <|> (reserved "multiset"    $> TokMultiset)
      <|> (reserved "seq"         $> TokSeq)
      <|> (reserved "rel"         $> TokRel)

      <|> (reserved "emptyset"    $> TokEmptySet)
      <|> (symbol   "\8709"       $> TokEmptySet) -- ∅
      <|> (symbol   "\\"          $> TokSetMinus)
      <|> (reserved "union"       $> TokSetUnion)
      <|> (symbol   "\8746"       $> TokSetUnion) -- ∪
      <|> (reserved "intersect"   $> TokSetIntersect)
      <|> (symbol   "\8745"       $> TokSetUnion) -- ∩

      <|> (reserved "new"         $> TokNew)
      <|> (reserved "free"        $> TokFree)
      -- V2.0

      <|> (symbol   "+"           $> TokPlus)
      <|> (symbol   "-"           $> TokMinus)
      <|> (symbol   "*"           $> TokTimes)
      <|> (symbol   "\215"        $> TokTimes) -- ×
      <|> (symbol   "/"           $> TokDiv)
      <|> (symbol   "\247"        $> TokDiv)   -- ÷
      <|> (reserved "mod"         $> TokMod)
      <|> (symbol   "^"           $> TokPower)

      <|> (reserved "abs"         $> TokAbs)
      <|> (reserved "sqrt"        $> TokSqrt)
      <|> (symbol   "\8730"       $> TokSqrt) -- √

      <|> (symbol   "==="         $> TokBEQ)
      <|> (symbol   "\8801"       $> TokBEQ)   -- ≡
      <|> (symbol   "!=="         $> TokBNE)
      <|> (symbol   "\8802"       $> TokBNE)   -- ≢

      <|> (symbol   "=="          $> TokAEQ)
      <|> (symbol   "!="          $> TokANE)
      <|> (symbol   "\8800"       $> TokANE)   -- ≠

      <|> (symbol   "<="          $> TokLE)
      <|> (symbol   "\8804"       $> TokLE)   -- ≤
      <|> (symbol   ">="          $> TokGE)
      <|> (symbol   "\8805"       $> TokGE)   -- ≥
      <|> (symbol   "<"           $> TokLT)
      <|> (symbol   ">"           $> TokGT)

      <|> (symbol   "!"           $> TokNot)
      <|> (symbol   "\172"        $> TokNot)  -- ¬

      <|> (symbol   "==>"         $> TokImplies)
      <|> (symbol   "\8658"       $> TokImplies)    -- ⇒
      <|> (symbol   "<=="         $> TokConsequent)
      <|> (symbol   "\8656"       $> TokConsequent) -- ⇐

      <|> (symbol   "(%"          $> TokLeftPercent)
      <|> (symbol   "%)"          $> TokRightPercent)

      <|> (symbol   "("           $> TokLeftPar)
      <|> (symbol   ")"           $> TokRightPar)

      <|> (symbol   "[]"          $> TokSepGuards)

      <|> (symbol   "|["          $> TokOpenBlock)
      <|> (symbol   "]|"          $> TokCloseBlock)

      <|> (symbol   "\10214"      $> TokOpenBlock)  -- ⟦
      <|> (symbol   "\10215"      $> TokCloseBlock) -- ⟧

      <|> (symbol   "["           $> TokLeftBracket)
      <|> (symbol   "]"           $> TokRightBracket)

      <|> (reserved "{pre"        $> TokLeftPre)
      <|> (symbol   "pre}"        $> TokRightPre)

      <|> (reserved "{post"       $> TokLeftPost)
      <|> (symbol   "post}"       $> TokRightPost)

      <|> (reserved "{bound"      $> TokLeftBound)
      <|> (symbol   "bound}"      $> TokRightBound)

      <|> (reserved "{a"          $> TokLeftA)
      <|> (symbol   "a}"          $> TokRightA)

      <|> (reserved "{inv"        $> TokLeftInv)
      <|> (symbol   "inv}"        $> TokRightInv)

      <|> (symbol   "{"           $> TokLeftBrace)
      <|> (symbol   "}"           $> TokRightBrace)

      <|> (symbol   "|"           $> TokPipe)

      <|> (reserved "max"         $> TokMax)
      <|> (reserved "min"         $> TokMin)
      <|> (reserved "forall"      $> TokForall)
      <|> (symbol   "\8704"       $> TokForall)   -- ∀
      <|> (reserved "exist"       $> TokExist)
      <|> (symbol   "\8707"       $> TokExist)    -- ∃
      <|> (reserved "notexist"    $> TokNotExist)
      <|> (symbol   "\8708"       $> TokNotExist) -- ∄
      <|> (reserved "sigma"       $> TokSigma)
      <|> (symbol   "\8721"       $> TokSigma)    -- ∑
      <|> (reserved "pi"          $> TokPi)
      <|> (symbol   "\8719"       $> TokPi)       -- ∏
      <|> (reserved "count"       $> TokCount)
      <|> (symbol   "#"           $> TokCount)

      <|> (reserved "if"          $> TokIf)
      <|> (reserved "fi"          $> TokFi)

      <|> (reserved "do"          $> TokDo)
      <|> (reserved "od"          $> TokOd)

      <|> (reserved "abort"       $> TokAbort)
      <|> (reserved "skip"        $> TokSkip)

      <|> (reserved "random"      $> TokRandom)
      <|> (reserved "write"       $> TokWrite)
      <|> (reserved "writeln"     $> TokWriteln)
      <|> (reserved "read"        $> TokRead)

      <|> (reserved "toChar"      $> TokToChar)
      <|> (reserved "toInt"       $> TokToInt)
      <|> (reserved "toDouble"    $> TokToDouble)

      <|> (reserved "true"        $> TokBool True)
      <|> (reserved "false"       $> TokBool False)

      <|> charLit
      <|> try floatLit
      <|> intLit
      <|> stringLit
      <|> identifier


tokenPos :: Lexer TokenPos
tokenPos = (flip . TokenPos) <$> getPosition <*> hidden token <*> getPosition


lexer :: Lexer [TokenPos]
lexer = between sc eof (many tokenPos)
