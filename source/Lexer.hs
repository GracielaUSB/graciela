module Lexer where

import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import Token

pPlus        = oneOf "+\43"
pMinus       = oneOf "-\45"
pStar        = oneOf "*\215"
pSlash       = oneOf "/\247"
pComma       = oneOf ",\44"
pLeftParent  = oneOf "(\40"
pRightParent = oneOf ")\41"
pProgram     = string "program"
pPre         = string "pre"
pPost        = string "post"
pBound       = string "bound"
pFunc        = string "func"
pProc        = string "proc"
pIn          = string "in"
pOut         = string "out"
pInOut       = string "inout"
pWith        = string "with"
pMod         = string "mod"
pMax         = string "max"
pMin         = string "min"
pForall      = string "forall"   <|> string "\8704" 
pExist       = string "exist"    <|> string "\8707" 
pNotExist    = string "notExist" <|> string "\8708"
pSigma       = string "sigma"    <|> string "\8721" 
pPi          = string "pi"       <|> string "\960"
pUnion       = string "union"    <|> string "\8746"
pIf          = string "if"
pFi          = string "fi"
pInv         = string "inv"
pDo          = string "do"
pOd          = string "od"
pGcd         = string "gcd"
pAbs         = string "abs"
pSqrt        = string "sqrt" <|> string "\8721" 
pLength      = string "length"
pVar         = string "var"
pConst       = string "const"
pAbort       = string "abort"
pRandom      = string "random"
pSkip        = string "skip"
pWrite       = string "write"
pWriteln     = string "writeln"
pRead        = string "read"
pToInt       = string "toInt"
pToDouble    = string "toDouble"
pToChar      = string "toChar"
pToString    = string "toString"
pType        = string "boolean"  <|> string "int"    <|> string "double"
               <|> string "char" <|> string "string" <|> string "array"  
pBool        = string "True"     <|> string "False"
pMIN_INT     = string "MIN_INT"
pMIN_DOUBLE  = string "MIN_DOUBLE"
pMAX_INT     = string "MAX_INT"
pMAX_DOUBLE  = string "MAX_DOUBLE"



lexer :: Parsec T.Text () ([TokenPos])
lexer = do pos <- getPosition    
           do  (eof >> spaces >> return ([(TokEnd, pos)]))
               <|> (do tok <- (   (pPlus        >> spaces >> return (TokPlus))
                              <|> (pMinus       >> spaces >> return (TokMinus))
                              <|> (pStar        >> spaces >> return (TokStar))
                              <|> (pSlash       >> spaces >> return (TokSlash))
                              <|> (pComma       >> spaces >> return (TokComma))
                              <|> (pLeftParent  >> spaces >> return (TokLeftParent))
                              <|> (pRightParent >> spaces >> return (TokRightParent))
                              <|> (pProgram     >> spaces >> return (TokProgram))
                              <|> (pPre         >> spaces >> return (TokPre))
                              <|> (pPost        >> spaces >> return (TokPost))
                              <|> (pBound       >> spaces >> return (TokBound))
                              <|> (pFunc        >> spaces >> return (TokFunc))
                              <|> (pProc        >> spaces >> return (TokProc))
                              <|> (pIn          >> spaces >> return (TokIn))
                              <|> (pOut         >> spaces >> return (TokOut))
                              <|> (pInOut       >> spaces >> return (TokInOut))
                              <|> (pWith        >> spaces >> return (TokWith))
                              <|> (pMod         >> spaces >> return (TokMod))
                              <|> (pMax         >> spaces >> return (TokMax))
                              <|> (pMin         >> spaces >> return (TokMin))
                              <|> (pForall      >> spaces >> return (TokForall))
                              <|> (pExist       >> spaces >> return (TokExist))
                              <|> (pNotExist    >> spaces >> return (TokNotExist))
                              <|> (pSigma       >> spaces >> return (TokSigma))
                              <|> (pPi          >> spaces >> return (TokPi))
                              <|> (pUnion       >> spaces >> return (TokUnion))
                              <|> (pIf          >> spaces >> return (TokIf))
                              <|> (pFi          >> spaces >> return (TokFi))
                              <|> (pInv         >> spaces >> return (TokInv))
                              <|> (pDo          >> spaces >> return (TokDo))  
                              <|> (pOd          >> spaces >> return (TokOd))
                              <|> (pGcd         >> spaces >> return (TokGcd))
                              <|> (pAbs         >> spaces >> return (TokAbs))
                              <|> (pSqrt        >> spaces >> return (TokSqrt))
                              <|> (pLength      >> spaces >> return (TokLength))
                              <|> (pVar         >> spaces >> return (TokVar))
                              <|> (pConst       >> spaces >> return (TokConst))
                              <|> (pAbort       >> spaces >> return (TokAbort))
                              <|> (pRandom      >> spaces >> return (TokRandom))
                              <|> (pSkip        >> spaces >> return (TokSkip))
                              <|> (pWrite       >> spaces >> return (TokWrite))
                              <|> (pWriteln     >> spaces >> return (TokWriteln))
                              <|> (pRead        >> spaces >> return (TokRead))
                              <|> (pToInt       >> spaces >> return (TokToInt))
                              <|> (pToDouble    >> spaces >> return (TokToDouble))    
                              <|> (pToChar      >> spaces >> return (TokToChar))
                              <|> (pToString    >> spaces >> return (TokToString))
                              <|> (pType        >> spaces >> return (TokType))
                              <|> (pBool        >> spaces >> return (TokBool))
                              <|> (pMIN_INT     >> spaces >> return (TokMIN_INT))
                              <|> (pMIN_DOUBLE  >> spaces >> return (TokMIN_DOUBLE))
                              <|> (pMAX_INT     >> spaces >> return (TokMAX_INT))
                              <|> (pMAX_DOUBLE  >> spaces >> return (TokMAX_DOUBLE))
                              <|> ((many1 digit)  AP.<* spaces >>= return . (TokInteger . read))
                              <|> (anyChar        AP.<* spaces >>= return . (TokError . T.singleton)))
                       fmap ((tok, pos) :) lexer)


