{-|
Module      : Lexer
Description : Analizador lexicografico
Copyright   : GraCieLa

Modulo del analizador lexicografico,
retorna una lista con los tokens de las palabras reservada.s.
-}
module Lexer where
--------------------------------------------------------------------------------
import           Token
import           Type
--------------------------------------------------------------------------------
import           Data.Functor.Identity (Identity)
import           Data.Text             (Text, cons, pack)
import           Text.Parsec
--------------------------------------------------------------------------------

-- | Intenta leer una palabra reservada o un identificador.
tryR :: String -> ParsecT Text () Identity String
tryR s = try $ do
    n <- string s
    notFollowedBy $ alphaNum <|> char '_' <|> char '?'
    return n

-- | Intenta leer un símbolo.
tryS :: String -> ParsecT Text () Identity String
tryS = try . string

-- | Es usada para los comentarios del lenguaje, por lo que toda la linea se ignora.
pComment :: ParsecT Text () Identity ()
pComment = optional . many $
    try (string "//") >> manyTill anyChar (lookAhead newline) >> spaces

lex1 :: ParsecT Text () Identity Token
lex1 =  (tryR "program"    >> spaces >> return TokProgram)
    <|> (tryR "begin"      >> spaces >> return TokBegin)
    <|> (tryR "end"        >> spaces >> return TokEnd)
    <|> (eof               >> spaces >> return TokEOF)

    <|> (tryR "func"       >> spaces >> return TokFunc)
    <|> (tryR "proc"       >> spaces >> return TokProc)
    <|> (tryR "in"         >> spaces >> return TokIn)
    <|> (tryR "out"        >> spaces >> return TokOut)
    <|> (tryR "inout"      >> spaces >> return TokInOut)
    <|> (tryR "ref"        >> spaces >> return TokRef)

    <|> (tryS ":="         >> spaces >> return TokAsig)
    <|> (tryS "\8788"      >> spaces >> return TokAsig)

    <|> (char ','          >> spaces >> return TokComma)
    <|> (char ':'          >> spaces >> return TokColon)
    <|> (char ';'          >> spaces >> return TokSemicolon)
    <|> (tryS "->"         >> spaces >> return TokArrow)
    <|> (tryS "\8594"      >> spaces >> return TokArrow)

    <|> (tryR "with"       >> spaces >> return TokWith)

    <|> (tryR "var"        >> spaces >> return TokVar)
    <|> (tryR "const"      >> spaces >> return TokConst)
    <|> (tryR "of"         >> spaces >> return TokOf)
    <|> (tryR "array"      >> spaces >> return TokArray)
    <|> (tryR "boolean"    >> spaces >> return (TokType GBool))
    <|> (tryR "int"        >> spaces >> return (TokType GInt))
    <|> (tryR "double"     >> spaces >> return (TokType GFloat))
    <|> (tryR "char"       >> spaces >> return (TokType GChar))

    <|> (tryS "/\\"        >> spaces >> return TokAnd)
    <|> (tryS "\8743"      >> spaces >> return TokAnd)
    <|> (tryS "\\/"        >> spaces >> return TokOr)
    <|> (tryS "\8744"      >> spaces >> return TokOr)

    <|> (char '+'          >> spaces >> return TokPlus)
    <|> (char '-'          >> spaces >> return TokMinus)
    <|> (char '*'          >> spaces >> return TokTimes)
    <|> (char '\215'       >> spaces >> return TokTimes)
    <|> (char '/'          >> spaces >> return TokDiv)
    <|> (char '\247'       >> spaces >> return TokDiv)
    <|> (tryR "mod"        >> spaces >> return TokMod)
    <|> (char '^'          >> spaces >> return TokPower)

    <|> (tryR "abs"        >> spaces >> return TokAbs)
    <|> (tryR "sqrt"       >> spaces >> return TokSqrt)
    <|> (tryS "\8730"      >> spaces >> return TokSqrt)

    <|> (tryS "=="         >> spaces >> return TokEQ)
    <|> (tryS "\8801"      >> spaces >> return TokEQ)
    <|> (tryS "!="         >> spaces >> return TokNE)
    <|> (tryS "\8800"      >> spaces >> return TokNE)
    <|> (tryS "\8802"      >> spaces >> return TokNE)
    <|> (tryS "<="         >> spaces >> return TokLE)
    <|> (tryS "\8804"      >> spaces >> return TokLE)
    <|> (tryS ">="         >> spaces >> return TokGE)
    <|> (tryS "\8805"      >> spaces >> return TokGE)
    <|> (char '<'          >> spaces >> return TokLT)
    <|> (char '>'          >> spaces >> return TokGT)

    <|> (char '!'          >> spaces >> return TokNot)

    <|> (tryS "==>"        >> spaces >> return TokImplies)
    <|> (tryS "\8658"      >> spaces >> return TokImplies)
    <|> (tryS "<=="        >> spaces >> return TokConsequent)
    <|> (tryS "\8656"      >> spaces >> return TokConsequent)

    <|> (tryS "(%"         >> spaces >> return TokLeftPercent)
    <|> (tryS "%)"         >> spaces >> return TokRightPercent)

    <|> (char '('          >> spaces >> return TokLeftPar)
    <|> (char ')'          >> spaces >> return TokRightPar)

    <|> (tryS "[]"         >> spaces >> return TokSepGuards)

    <|> (tryS "|["         >> spaces >> return TokOpenBlock)
    <|> (tryS "]|"         >> spaces >> return TokCloseBlock)

    <|> (char '['          >> spaces >> return TokLeftBracket)
    <|> (char ']'          >> spaces >> return TokRightBracket)

    <|> (tryS "{pre"       >> spaces >> return TokLeftPre)
    <|> (tryS "pre}"       >> spaces >> return TokRightPre)

    <|> (tryS "{post"      >> spaces >> return TokLeftPost)
    <|> (tryS "post}"      >> spaces >> return TokRightPost)

    <|> (tryS "{bound"     >> spaces >> return TokLeftBound)
    <|> (tryS "bound}"     >> spaces >> return TokRightBound)

    <|> (tryS "{a"         >> spaces >> return TokLeftA)
    <|> (tryS "a}"         >> spaces >> return TokRightA)

    <|> (tryS "{inv"       >> spaces >> return TokLeftInv)
    <|> (tryS "inv}"       >> spaces >> return TokRightInv)

    <|> (char '{'          >> spaces >> return TokLeftBrace)
    <|> (char '}'          >> spaces >> return TokRightBrace)

    <|> (tryS "|"          >> spaces >> return TokPipe)

    <|> (tryR "max"        >> spaces >> return TokMax)
    <|> (tryR "min"        >> spaces >> return TokMin)
    <|> (tryR "forall"     >> spaces >> return TokForall)
    <|> (tryS "\8704"      >> spaces >> return TokForall)
    <|> (tryR "exist"      >> spaces >> return TokExist)
    <|> (tryS "\8707"      >> spaces >> return TokExist)
    <|> (tryR "not-exist"  >> spaces >> return TokNotExist)
    <|> (tryS "\8708"      >> spaces >> return TokNotExist)
    <|> (tryR "sigma"      >> spaces >> return TokSigma)
    <|> (tryS "\8721"      >> spaces >> return TokSigma)
    <|> (tryR "pi"         >> spaces >> return TokPi)
    <|> (tryS "\8719"      >> spaces >> return TokPi)

    <|> (tryR "if"         >> spaces >> return TokIf)
    <|> (tryR "fi"         >> spaces >> return TokFi)

    <|> (tryR "do"         >> spaces >> return TokDo)
    <|> (tryR "od"         >> spaces >> return TokOd)

    <|> (tryR "abort"      >> spaces >> return TokAbort)
    <|> (tryR "skip"       >> spaces >> return TokSkip)

    <|> (tryR "random"     >> spaces >> return TokRandom)
    <|> (tryR "write"      >> spaces >> return TokWrite)
    <|> (tryR "writeln"    >> spaces >> return TokWriteln)
    <|> (tryR "read"       >> spaces >> return TokRead)

    <|> (tryR "toChar"     >> spaces >> return TokToChar)
    <|> (tryR "toInt"      >> spaces >> return TokToInt)
    <|> (tryR "toDouble"   >> spaces >> return TokToDouble)

    <|> (tryR "MIN_INT"    >> spaces >> return TokMinInt)
    <|> (tryR "MIN_DOUBLE" >> spaces >> return TokMinDouble)
    <|> (tryR "MAX_INT"    >> spaces >> return TokMaxInt)
    <|> (tryR "MAX_DOUBLE" >> spaces >> return TokMaxDouble)

    <|> (tryR "true"       >> spaces >> return (TokBool True))
    <|> (tryR "false"      >> spaces >> return (TokBool False))

    <|> (do _ <- char '\''
            c <- anyChar
            _ <- char '\''
            spaces
            return (TokChar c)
        )

    <|> ((TokInteger . read) <$> (many1 digit <* spaces))

    <|> try (do n1 <- many1 digit
                _  <- char '.'
                n2 <- many1 digit
                return (TokFloat (read (n1 ++ "." ++ n2)))
        )

    <|> (TokString <$> (char '"' *> manyTill anyChar (char '"') <* spaces))

    <|> try (do l <- letter
                r <- many (alphaNum <|> char '_' <|> char '?')
                spaces
                return (TokId (cons l (pack r)))
        )

    <|> (do c <- anyToken
            unexpected [c])


-- | Se encarga de generar la lista con todos los tokens.
lexer :: Parsec Text () [TokenPos]
lexer = do
    spaces
    pComment
    pos <- getPosition
    tok <- lex1
    if tok == TokEOF
        then return [(tok, pos)]
        else ((tok, pos) :) <$> lexer
