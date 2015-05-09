module TokenParser where

import Token
import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import Type
import State

makeTokenParser x = tokenPrim showTok posTok testTok
                    where
                      showTok (t, pos)     = show t
                      posTok  _ (_ , pos) _ = pos
                      testTok (t, pos)     = if x == t then Just (t) else Nothing

verify :: Token ->  MyParser (Token)
verify token = makeTokenParser token

parsePlus         = verify TokPlus
parseMinus        = verify TokMinus
parseSlash        = verify TokSlash
parseStar         = verify TokStar
parseComma        = verify TokComma
parseLeftParent   = verify TokLeftParent
parseRightParent  = verify TokRightParent
parseEnd          = verify TokEnd
parseMaxInt       = verify TokMAX_INT
parseMinInt       = verify TokMIN_INT
parseMaxDouble    = verify TokMAX_DOUBLE
parseMinDouble    = verify TokMIN_DOUBLE
parseProgram      = verify TokProgram
parseLBracket     = verify TokLeftBracket
parseRBracket     = verify TokRightBracket
parseToInt        = verify TokToInt
parseToDouble     = verify TokToDouble
parseToChar       = verify TokToChar
parseToString     = verify TokToString
parseLeftBracket  = verify TokLeftBracket
parseRightBracket = verify TokRightBracket
parseTokAbs       = verify TokAbs
parseTokSqrt      = verify TokSqrt
parseTokAccent    = verify TokAccent
parseAnd          = verify TokLogicalAnd
parseOr           = verify TokLogicalOr
parseNotEqual     = verify TokNotEqual
parseEqual        = verify TokEquiv
parseSkip         = verify TokSkip        
parseIf           = verify TokIf
parseFi           = verify TokFi
parseAbort        = verify TokAbort
parseWrite        = verify TokWrite
parseSemicolon    = verify TokSemicolon
parseArrow        = verify TokArrow
parseSepGuards    = verify TokSepGuards
parseDo           = verify TokDo
parseOd           = verify TokOd
parseAssign       = verify TokAsig
parseRandom       = verify TokRandom
parseTokOpenBlock = verify TokOpenBlock
parseTokCloseBlock= verify TokCloseBlock
parseColon        = verify TokColon
parseVar          = verify TokVar
parseConst        = verify TokConst
parseFunc         = verify TokFunc
parseProc         = verify TokProc
parseIn           = verify TokIn
parseOut          = verify TokOut
parseInOut        = verify TokInOut
parseRead         = verify TokRead
parseWith         = verify TokWith
parseWriteln      = verify TokWriteln
parseOf           = verify TokOf 
parseTokArray     = verify TokArray
parseTokPre       = verify TokPre
parseTokLeftPre   = verify TokLeftPre
parseTokRightPre  = verify TokRightPre
parseTokLeftPost  = verify TokLeftPost
parseTokRightPost = verify TokRightPost
parseTokExist     = verify TokExist
parseTokOpenQuant = verify TokOpenQuant
parseTokLeftPer   = verify TokLeftPercent
parseTokRightPer  = verify TokRightPercent
parseTokLeftBound = verify TokLeftBound
parseTokRightBound= verify TokRightBound
parseTokLeftA     = verify TokLeftA
parseTokRightA    = verify TokRightA
parseTokLeftInv   = verify TokLeftInv
parseTokRightInv  = verify TokRightInv
parseTokLength    = verify TokLength
parseTokNot       = verify TokNot
parseTokLEqual    = verify TokLessEqual
parseTokGEqual    = verify TokGreaterEqual
parseTokLess      = verify TokLess
parseTokGreater   = verify TokGreater
parseTokImplies   = verify TokImplies
parseTokConse     = verify TokConsequent
parseTokMod       = verify TokMod
parseTokMax       = verify TokMax
parseTokMin       = verify TokMin
parseTokForall    = verify TokForall
parseTokNotExist  = verify TokNotExist
parseTokSigma     = verify TokSigma
parseTokPi        = verify TokPi
parseTokUnion     = verify TokUnion
parseTokEqual     = verify TokEqual

parseID :: MyParser (Token)
parseID = tokenPrim showTok posTok testTok
          where
            showTok (t, pos) = show t
            posTok  _ (t, pos) _ = pos
            testTok (t, pos) = case t of
                                 { TokId id  -> Just (TokId id)
                                 ; otherwise -> Nothing
                                 }

parseBool :: MyParser (Token)
parseBool = tokenPrim showTok posTok testTok
            where
              showTok (t, pos) = show t
              posTok  _ (t, pos) _ = pos
              testTok (t, pos) = case t of
                                  { TokBool b -> Just (TokBool b)
                                  ; otherwise -> Nothing
                                  }

parseType :: MyParser (Token)
parseType = tokenPrim showTok posTok testTok
              where
                showTok (t, pos) = show t
                posTok  _ (t, pos) _ = pos
                testTok (t, pos) = case t of
                                    { TokType b -> Just (TokType b)
                                    ; otherwise -> Nothing
                                    }

parseChar :: MyParser (Token)
parseChar = tokenPrim showTok posTok testTok
            where
              showTok (t, pos) = show t
              posTok  _ (t, pos) _ = pos
              testTok (t, pos) = case t of
                                  { TokChar b -> Just (TokChar b)
                                  ; otherwise -> Nothing
                                  }

parseString :: MyParser (Token)
parseString = tokenPrim showTok posTok testTok
                where
                  showTok (t, pos) = show t
                  posTok  _ (t, pos) _ = pos
                  testTok (t, pos) = case t of
                                      { TokString b -> Just (TokString b)
                                      ; otherwise   -> Nothing
                                      }

parseAnyToken :: MyParser (Token)
parseAnyToken = tokenPrim showTok posTok testTok
                where
                  showTok (t, pos) = show t
                  posTok  _ (t, pos) _ = pos
                  testTok (t, pos) = Just (t)

number :: MyParser (Token)
number = tokenPrim showTok posTok testTok
         where
           showTok (t, pos)     = show t
           posTok  _ (t, pos) _ = pos
           testTok (t, pos)     = case t of
                                  { TokInteger n -> Just (TokInteger n)
                                  ; otherwise    -> Nothing
                                  }
