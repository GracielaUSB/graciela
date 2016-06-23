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
lex1 =  (tryR "program"    >> return TokProgram)
    <|> (tryR "begin"      >> return TokBegin)
    <|> (tryR "end"        >> return TokEnd)
    <|> (eof               >> return TokEOF)

    <|> (tryR "func"       >> return TokFunc)
    <|> (tryR "proc"       >> return TokProc)
    <|> (tryR "in"         >> return TokIn)
    <|> (tryR "out"        >> return TokOut)
    <|> (tryR "inout"      >> return TokInOut)
    <|> (tryR "ref"        >> return TokRef)

    <|> (tryS ":="         >> return TokAsig)
    <|> (tryS "\8788"      >> return TokAsig)

    <|> (char ','          >> return TokComma)
    <|> (char ':'          >> return TokColon)
    <|> (char ';'          >> return TokSemicolon)
    <|> (tryS "->"         >> return TokArrow)
    <|> (tryS "\8594"      >> return TokArrow)

    <|> (tryR "with"       >> return TokWith)

    -- V2.0
    <|> (tryR "type"       >> return TokTypeImplements)
    <|> (tryR "implements" >> return TokImplements)
    <|> (tryR "abstract"   >> return TokAbstract)
    <|> (tryR "{repinv"    >> return TokLeftRep)    
    <|> (tryR "repinv}"    >> return TokRightRep)   
    <|> (tryR "{acinv"     >> return TokLeftAcopl)    
    <|> (tryR "acinv}"     >> return TokRightAcopl) 
    -- V2.0

    <|> (tryR "var"        >> return TokVar)
    <|> (tryR "const"      >> return TokConst)
    <|> (tryR "of"         >> return TokOf)
    <|> (tryR "array"      >> return TokArray)
    <|> (tryR "boolean"    >> return (TokType GBool))
    <|> (tryR "int"        >> return (TokType GInt))
    <|> (tryR "double"     >> return (TokType GFloat))
    <|> (tryR "char"       >> return (TokType GChar))

    <|> (tryS "/\\"        >> return TokAnd)
    <|> (tryS "\8743"      >> return TokAnd)
    <|> (tryS "\\/"        >> return TokOr)
    <|> (tryS "\8744"      >> return TokOr)

    <|> (char '+'          >> return TokPlus)
    <|> (char '-'          >> return TokMinus)
    <|> (char '*'          >> return TokTimes)
    <|> (char '\215'       >> return TokTimes)
    <|> (char '/'          >> return TokDiv)
    <|> (char '\247'       >> return TokDiv)
    <|> (tryR "mod"        >> return TokMod)
    <|> (char '^'          >> return TokPower)

    <|> (tryR "abs"        >> return TokAbs)
    <|> (tryR "sqrt"       >> return TokSqrt)
    <|> (tryS "\8730"      >> return TokSqrt)

    <|> (tryS "=="         >> return TokEQ)
    <|> (tryS "\8801"      >> return TokEQ)
    <|> (tryS "!="         >> return TokNE)
    <|> (tryS "\8800"      >> return TokNE)
    <|> (tryS "\8802"      >> return TokNE)
    <|> (tryS "<="         >> return TokLE)
    <|> (tryS "\8804"      >> return TokLE)
    <|> (tryS ">="         >> return TokGE)
    <|> (tryS "\8805"      >> return TokGE)
    <|> (char '<'          >> return TokLT)
    <|> (char '>'          >> return TokGT)

    <|> (char '!'          >> return TokNot)

    <|> (tryS "==>"        >> return TokImplies)
    <|> (tryS "\8658"      >> return TokImplies)
    <|> (tryS "<=="        >> return TokConsequent)
    <|> (tryS "\8656"      >> return TokConsequent)

    <|> (tryS "(%"         >> return TokLeftPercent)
    <|> (tryS "%)"         >> return TokRightPercent)

    <|> (char '('          >> return TokLeftPar)
    <|> (char ')'          >> return TokRightPar)

    <|> (tryS "[]"         >> return TokSepGuards)

    <|> (tryS "|["         >> return TokOpenBlock)
    <|> (tryS "]|"         >> return TokCloseBlock)

    <|> (char '['          >> return TokLeftBracket)
    <|> (char ']'          >> return TokRightBracket)

    <|> (tryR "{pre"       >> return TokLeftPre)
    <|> (tryS "pre}"       >> return TokRightPre)

    <|> (tryR "{post"      >> return TokLeftPost)
    <|> (tryS "post}"      >> return TokRightPost)

    <|> (tryR "{bound"     >> return TokLeftBound)
    <|> (tryS "bound}"     >> return TokRightBound)

    <|> (tryR "{a"         >> return TokLeftA)
    <|> (tryS "a}"         >> return TokRightA)

    <|> (tryR "{inv"       >> return TokLeftInv)
    <|> (tryS "inv}"       >> return TokRightInv)

    <|> (char '{'          >> return TokLeftBrace)
    <|> (char '}'          >> return TokRightBrace)

    <|> (tryS "|"          >> return TokPipe)

    <|> (tryR "max"        >> return TokMax)
    <|> (tryR "min"        >> return TokMin)
    <|> (tryR "forall"     >> return TokForall)
    <|> (tryS "\8704"      >> return TokForall)
    <|> (tryR "exist"      >> return TokExist)
    <|> (tryS "\8707"      >> return TokExist)
    <|> (tryR "not-exist"  >> return TokNotExist)
    <|> (tryS "\8708"      >> return TokNotExist)
    <|> (tryR "sigma"      >> return TokSigma)
    <|> (tryS "\8721"      >> return TokSigma)
    <|> (tryR "pi"         >> return TokPi)
    <|> (tryS "\8719"      >> return TokPi)

    <|> (tryR "if"         >> return TokIf)
    <|> (tryR "fi"         >> return TokFi)

    <|> (tryR "do"         >> return TokDo)
    <|> (tryR "od"         >> return TokOd)

    <|> (tryR "abort"      >> return TokAbort)
    <|> (tryR "skip"       >> return TokSkip)

    <|> (tryR "random"     >> return TokRandom)
    <|> (tryR "write"      >> return TokWrite)
    <|> (tryR "writeln"    >> return TokWriteln)
    <|> (tryR "read"       >> return TokRead)

    <|> (tryR "toChar"     >> return TokToChar)
    <|> (tryR "toInt"      >> return TokToInt)
    <|> (tryR "toDouble"   >> return TokToDouble)

    <|> (tryR "MIN_INT"    >> return TokMinInt)
    <|> (tryR "MIN_DOUBLE" >> return TokMinDouble)
    <|> (tryR "MAX_INT"    >> return TokMaxInt)
    <|> (tryR "MAX_DOUBLE" >> return TokMaxDouble)

    <|> (tryR "true"       >> return (TokBool True))
    <|> (tryR "false"      >> return (TokBool False))

    <|> (do _ <- char '\''
            c <- anyChar
            _ <- char '\''
            return (TokChar c)
        )

    <|> ((TokInteger . read) <$> (many1 digit))

    <|> try (do n1 <- many1 digit
                _  <- char '.'
                n2 <- many1 digit
                return (TokFloat (read (n1 ++ "." ++ n2)))
        )

    <|> (TokString <$> (char '"' *> manyTill anyChar (char '"') ))

    <|> try (do l <- letter
                r <- many (alphaNum <|> char '_' <|> char '?')
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
