{-|
Module      : Lexer
Description : Analizador lexicografico
Copyright   : GraCieLa

Modulo del analizador lexicografico, 
retorna una lista con los tokens de las palabras reservada.s.
-}
module Lexer where

import qualified Control.Applicative as AP
import qualified Data.Text           as T
import Data.Functor.Identity         
import Text.Parsec
import Token
import Type


-- | Intenta leer una palabra reservada o un identificador.
tryString :: String -> ParsecT T.Text () Identity String
tryString s = 
  try $ do n <- string s
           notFollowedBy $ alphaNum <|> char '_' <|> char '?'
           return n


-- | Intenta leer un operador.
tryStringOp :: String -> ParsecT T.Text () Identity String
tryStringOp = try . string


-- | La función 'pPlus' revisa la palabra reservada.
pPlus         = oneOf "+\43"

-- | La función 'pMinus' revisa la palabra reservada.
pMinus        = oneOf "-\45"

-- | La función 'pStar' revisa la palabra reservada.
pStar         = oneOf "*\215"

-- | La función 'pSlash' revisa la palabra reservada.
pSlash        = oneOf "/\247"

-- | La función 'pComma' revisa la palabra reservada.
pComma        = oneOf ","

-- | La función 'pLeftParent' revisa la palabra reservada.
pLeftParent   = oneOf "("

-- | La función 'pRightParent' revisa la palabra reservada.
pRightParent  = oneOf ")"

-- | La función 'pLeftBracket' revisa la palabra reservada.
pLeftBracket  = oneOf "["

-- | La función 'pRightBracket' revisa la palabra reservada.
pRightBracket = oneOf "]"

-- | La función 'pSemicolon' revisa la palabra reservada.
pSemicolon    = oneOf ";"

-- | La función 'pColon' revisa la palabra reservada.
pColon        = oneOf ":" 

-- | La función 'pLeftBrace' revisa la palabra reservada.
pLeftBrace    = oneOf "{"

-- | La función 'pRightBrace' revisa la palabra reservada.
pRightBrace   = oneOf "}"

-- | La función 'pLess' revisa la palabra reservada.
pLess         = oneOf "<\60"

-- | La función 'pGreater' revisa la palabra reservada.
pGreater      = oneOf ">\62"

-- | La función 'pNot' revisa la palabra reservada.
pNot          = oneOf "!\33"

-- | La función 'pAccent' revisa la palabra reservada.
pAccent       = oneOf "^\94"

-- | La función 'pPipe' revisa la palabra reservada.
pPipe         = tryStringOp "|"

-- | La función 'pOpenBlock' revisa la palabra reservada.
pOpenBlock    = tryStringOp "|["

-- | La función 'pCloseBlock' revisa la palabra reservada.
pCloseBlock   = tryStringOp "]|"

-- | La función 'pSepGuards' revisa la palabra reservada.
pSepGuards    = tryStringOp "[]"

-- | La función 'pLogicalAnd' revisa la palabra reservada.
pLogicalAnd   = tryStringOp "/\\" <|> tryString "\8743" 

-- | La función 'pLogicalOr' revisa la palabra reservada.
pLogicalOr    = tryStringOp "\\/" <|> tryString "\8744"

-- | La función 'pNotEqual' revisa la palabra reservada.
pNotEqual     = tryStringOp "!="  <|> tryString "\8800"

-- | La función 'pLessEqual' revisa la palabra reservada.
pLessEqual    = tryStringOp "<="  <|> tryString "\8804"

-- | La función 'pGreaterEqual' revisa la palabra reservada.
pGreaterEqual = tryStringOp ">="  <|> tryString "\8805"

-- | La función 'pImplies' revisa la palabra reservada.
pImplies      = tryStringOp "==>" <|> tryString "\8658"

-- | La función 'pConsequent' revisa la palabra reservada.
pConsequent   = tryStringOp "<==" <|> tryString "\8656"

-- | La función 'pEquiv' revisa la palabra reservada.
pEquiv        = tryStringOp "=="  <|> tryString "\8801"

-- | La función 'pAsig' revisa la palabra reservada.
pAsig         = tryStringOp ":="  <|> tryString "\58\61" 

-- | La función 'pArrow' revisa la palabra reservada.
pArrow        = tryStringOp "->"  <|> tryString "\8594"

-- | La función 'pLeftPercent' revisa la palabra reservada.
pLeftPercent  = tryStringOp "(%"  

-- | La función 'pRightPercent' revisa la palabra reservada.
pRightPercent = tryStringOp "%)"  

-- | La función 'pLeftPre' revisa la palabra reservada.
pLeftPre      = tryStringOp "{pre"

-- | La función 'pRightPre' revisa la palabra reservada.
pRightPre     = tryStringOp "pre}"

-- | La función 'pLeftPost' revisa la palabra reservada.
pLeftPost     = tryStringOp "{post"

-- | La función 'pRightPost' revisa la palabra reservada.
pRightPost    = tryStringOp "post}"

-- | La función 'pLeftBound' revisa la palabra reservada.
pLeftBound    = tryStringOp "{bound"

-- | La función 'pRightBound' revisa la palabra reservada.
pRightBound   = tryStringOp "bound}"

-- | La función 'pLeftA' revisa la palabra reservada.
pLeftA        = tryStringOp "{a"

-- | La función 'pRightA' revisa la palabra reservada.
pRightA       = tryStringOp "a}"

-- | La función 'pLeftInv' revisa la palabra reservada.
pLeftInv      = tryStringOp "{inv"

-- | La función 'pRightInv' revisa la palabra reservada.
pRightInv     = tryStringOp "inv}"

-- | La función 'pFunc' revisa la palabra reservada.
pFunc         = tryString "func"

-- | La función 'pProc' revisa la palabra reservada.
pProc         = tryString "proc"

-- | La función 'pIn' revisa la palabra reservada.
pIn           = tryString "in"

-- | La función 'pOut' revisa la palabra reservada.
pOut          = tryString "out"

-- | La función 'pInOut' revisa la palabra reservada.
pInOut        = tryString "inout"

-- | La función 'pRef' revisa la palabra reservada.
pRef          = tryString "ref"

-- | La función 'pWith' revisa la palabra reservada.
pWith         = tryString "with"

-- | La función 'pMod' revisa la palabra reservada.
pMod          = tryString "mod"

-- | La función 'pMax' revisa la palabra reservada.
pMax          = tryString "max"

-- | La función 'pMin' revisa la palabra reservada.
pMin          = tryString "min"

-- | La función 'pForall' revisa la palabra reservada.
pForall       = tryString "forall"   <|> tryString "\8704"

-- | La función 'pExist' revisa la palabra reservada." 
pExist        = tryString "exist"    <|> tryString "\8707"

-- | La función 'pSigma' revisa la palabra reservada." 
pSigma        = tryString "sigma"    <|> tryString "\8721"

-- | La función 'pPi' revisa la palabra reservada." 
pPi           = tryString "pi"       <|> tryString "\960"

-- | La función 'pIf' revisa la palabra reservada.
pIf           = tryString "if"

-- | La función 'pFi' revisa la palabra reservada.
pFi           = tryString "fi"

-- | La función 'pDo' revisa la palabra reservada.
pDo           = tryString "do"

-- | La función 'pOd' revisa la palabra reservada.
pOd           = tryString "od"

-- | La función 'pAbs' revisa la palabra reservada.
pAbs          = tryString "abs"

-- | La función 'pSqrt' revisa la palabra reservada.
pSqrt         = tryString "sqrt"     <|> tryString "\8730"

-- | La función 'pVar' revisa la palabra reservada. 
pVar          = tryString "var"

-- | La función 'pConst' revisa la palabra reservada.
pConst        = tryString "const"

-- | La función 'pAbort' revisa la palabra reservada.
pAbort        = tryString "abort"

-- | La función 'pRandom' revisa la palabra reservada.
pRandom       = tryString "random"

-- | La función 'pSkip' revisa la palabra reservada.
pSkip         = tryString "skip"

-- | La función 'pWrite' revisa la palabra reservada.
pWrite        = tryString "write"

-- | La función 'pWriteln' revisa la palabra reservada.
pWriteln      = tryString "writeln"

-- | La función 'pRead' revisa la palabra reservada.
pRead         = tryString "read"

-- | La función 'pProgram' revisa la palabra reservada.
pProgram      = tryString "program"  

-- | La función 'pToInt' revisa la palabra reservada.
pToInt        = tryString "toInt"

-- | La función 'pToDouble' revisa la palabra reservada.
pToDouble     = tryString "toDouble"

-- | La función 'pToChar' revisa la palabra reservada.
pToChar       = tryString "toChar"

-- | La función 'pType' revisa la palabra reservada.
pType         = (tryString "boolean" >> return MyBool)
            <|> (tryString "int"     >> return MyInt)
            <|> (tryString "double"  >> return MyFloat)
            <|> (tryString "char"    >> return MyChar)

-- | La función 'pArray' revisa la palabra reservada.
pArray        = tryString "array"

-- | La función 'pBool' revisa la palabra reservada.
pBool         = tryString "true"     <|> tryString "false"

-- | La función 'pMIN_INT' revisa la palabra reservada.
pMIN_INT      = tryString "MIN_INT"

-- | La función 'pMIN_DOUBLE' revisa la palabra reservada.
pMIN_DOUBLE   = tryString "MIN_DOUBLE"

-- | La función 'pMAX_INT' revisa la palabra reservada.
pMAX_INT      = tryString "MAX_INT"

-- | La función 'pMAX_DOUBLE' revisa la palabra reservada.
pMAX_DOUBLE   = tryString "MAX_DOUBLE"

-- | La función 'pOf' revisa la palabra reservada.
pOf           = tryString "of"

-- | La función 'pBegin' revisa la palabra reservada.
pBegin        = tryString "begin"

-- | La función 'pEnd' revisa la palabra reservada.
pEnd          = tryString "end"


-- | Es usada para los comentarios del lenguaje, por lo que toda la linea se ignora.
pComment :: ParsecT T.Text () Identity ()
pComment = optional(do many (tryStringOp "//" >> manyTill anyChar (lookAhead (newline)) >> spaces))


-- | Se encarga de generar la lista con todos los tokens. 
lexer :: Parsec T.Text () [TokenPos]
lexer = 
    do spaces
       pComment
       pos <- getPosition
       do  (eof >> spaces >> return ([(TokEnd, pos)]))
           <|> (do tok <- (   (pPlus         >> spaces >> return (TokPlus))
                          <|> (pBegin        >> spaces >> return (TokBegin))
                          <|> (pEnd          >> spaces >> return (TokLexEnd))
                          <|> (pArrow        >> spaces >> return (TokArrow))
                          <|> (pLogicalAnd   >> spaces >> return (TokLogicalAnd))
                          <|> (pLeftPre      >> spaces >> return (TokLeftPre))
                          <|> (pRightPre     >> spaces >> return (TokRightPre))
                          <|> (pLeftPost     >> spaces >> return (TokLeftPost))
                          <|> (pRightPost    >> spaces >> return (TokRightPost))
                          <|> (pLeftBound    >> spaces >> return (TokLeftBound))
                          <|> (pRightBound   >> spaces >> return (TokRightBound))
                          <|> (pType         AP.<* spaces >>= return . TokType)
                          <|> (pLeftA        >> spaces >> return (TokLeftA))
                          <|> (pRightA       >> spaces >> return (TokRightA))
                          <|> (pLeftInv      >> spaces >> return (TokLeftInv))
                          <|> (pRightInv     >> spaces >> return (TokRightInv))
                          <|> (pMinus        >> spaces >> return (TokMinus))
                          <|> (pStar         >> spaces >> return (TokStar))
                          <|> (pSlash        >> spaces >> return (TokSlash))
                          <|> (pComma        >> spaces >> return (TokComma))
                          <|> (pLeftPercent  >> spaces >> return (TokLeftPercent))
                          <|> (pRightPercent >> spaces >> return (TokRightPercent))
                          <|> (pLeftParent   >> spaces >> return (TokLeftParent))
                          <|> (pRightParent  >> spaces >> return (TokRightParent))
                          <|> (pAccent       >> spaces >> return (TokAccent))
                          <|> (pLogicalOr    >> spaces >> return (TokLogicalOr))
                          <|> (pNotEqual     >> spaces >> return (TokNotEqual))
                          <|> (pImplies      >> spaces >> return (TokImplies))
                          <|> (pConsequent   >> spaces >> return (TokConsequent))
                          <|> (pLessEqual    >> spaces >> return (TokLessEqual))
                          <|> (pGreaterEqual >> spaces >> return (TokGreaterEqual))
                          <|> (pEquiv        >> spaces >> return (TokEquiv))
                          <|> (pArray        >> spaces >> return (TokArray))
                          <|> (pAsig         >> spaces >> return (TokAsig))
                          <|> (pLess         >> spaces >> return (TokLess))
                          <|> (pGreater      >> spaces >> return (TokGreater))
                          <|> (pNot          >> spaces >> return (TokNot))
                          <|> (pProgram      >> spaces >> return (TokProgram))
                          <|> (pOpenBlock    >> spaces >> return (TokOpenBlock))
                          <|> (pCloseBlock   >> spaces >> return (TokCloseBlock))
                          <|> (pPipe         >> spaces >> return (TokPipe))
                          <|> (pSepGuards    >> spaces >> return (TokSepGuards))
                          <|> (pLeftBracket  >> spaces >> return (TokLeftBracket))
                          <|> (pRightBracket >> spaces >> return (TokRightBracket))
                          <|> (pSemicolon    >> spaces >> return (TokSemicolon))
                          <|> (pColon        >> spaces >> return (TokColon))
                          <|> (pLeftBrace    >> spaces >> return (TokLeftBrace))
                          <|> (pRightBrace   >> spaces >> return (TokRightBrace))
                          <|> (pFunc         >> spaces >> return (TokFunc))
                          <|> (pProc         >> spaces >> return (TokProc))
                          <|> (pInOut        >> spaces >> return (TokInOut))    
                          <|> (pOut          >> spaces >> return (TokOut))
                          <|> (pRef          >> spaces >> return (TokRef))
                          <|> (pWith         >> spaces >> return (TokWith))
                          <|> (pMod          >> spaces >> return (TokMod))
                          <|> (pMax          >> spaces >> return (TokMax))
                          <|> (pMin          >> spaces >> return (TokMin))
                          <|> (pForall       >> spaces >> return (TokForall))
                          <|> (pExist        >> spaces >> return (TokExist))
                          <|> (pSigma        >> spaces >> return (TokSigma))
                          <|> (pPi           >> spaces >> return (TokPi))
                          <|> (pIf           >> spaces >> return (TokIf))
                          <|> (pFi           >> spaces >> return (TokFi))
                          <|> (pDo           >> spaces >> return (TokDo))  
                          <|> (pOd           >> spaces >> return (TokOd))
                          <|> (pAbs          >> spaces >> return (TokAbs))
                          <|> (pSqrt         >> spaces >> return (TokSqrt))
                          <|> (pVar          >> spaces >> return (TokVar))
                          <|> (pConst        >> spaces >> return (TokConst))
                          <|> (pAbort        >> spaces >> return (TokAbort))
                          <|> (pRandom       >> spaces >> return (TokRandom))
                          <|> (pSkip         >> spaces >> return (TokSkip))
                          <|> (pWriteln      >> spaces >> return (TokWriteln))
                          <|> (pWrite        >> spaces >> return (TokWrite))
                          <|> (pRead         >> spaces >> return (TokRead))
                          <|> (pToInt        >> spaces >> return (TokToInt))
                          <|> (pToDouble     >> spaces >> return (TokToDouble))    
                          <|> (pToChar       >> spaces >> return (TokToChar))
                          <|> (pIn           >> spaces >> return (TokIn))
                          <|> (pMIN_INT      >> spaces >> return (TokMIN_INT))
                          <|> (pMIN_DOUBLE   >> spaces >> return (TokMIN_DOUBLE))
                          <|> (pMAX_INT      >> spaces >> return (TokMAX_INT))
                          <|> (pMAX_DOUBLE   >> spaces >> return (TokMAX_DOUBLE))
                          <|> (pOf           >> spaces >> return (TokOf))
                          <|> (try (do s <- pBool
                                       spaces
                                       case s of
                                        { "true"  -> return $ TokBool True
                                        ; "false" -> return $ TokBool False
                                        }
                                    )
                              ) 
                          <|> ((char '"')    AP.*> manyTill anyChar (char '"') AP.<* spaces >>= return . TokString)
                          <|> (do char '\''
                                  c <- anyChar
                                  char '\''
                                  spaces
                                  return (TokChar c)
                              )
                          <|> (try( do n1 <- many1 digit
                                       char '.'
                                       n2 <- many1 digit
                                       return (TokFlotante (read (n1 ++ "." ++ n2))))
                              )                             
                          <|> ((many1 digit)  AP.<* spaces >>= return . (TokInteger . read))
                          <|> (try (do l <- letter
                                       r <- many (alphaNum <|> char '_' <|> char '?')
                                       spaces
                                       return $ TokId (T.cons l (T.pack r)) 
                                   )
                              )
                          )
                          <|> (do c <- anyToken
                                  unexpected [c])
                   fmap ((tok, pos) :) lexer)
