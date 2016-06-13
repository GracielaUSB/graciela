module TokenParser where
--------------------------------------------------------------------------------
import           State
import           Token
import           Type
--------------------------------------------------------------------------------
import           Data.Text   (Text)
import           Text.Parsec
--------------------------------------------------------------------------------

verify :: Token -> MyParser Token
verify x =
    tokenPrim showTok updatePos testTok
    where
        showTok (t, pos) = let line   = sourceLine pos
                               column = sourceColumn pos
                           in ":" ++ show t ++ ", en la línea " ++ show line
                                            ++ ", columna "     ++ show column ++ "."

        testTok (t, pos) = if x == t
            then Just t else Nothing

updatePos :: SourcePos -> (Token, SourcePos) -> [TokenPos] -> SourcePos
updatePos _ _ ((_, pos):xs) = pos
updatePos _ (_, pos) []     = pos

parseBegin        = verify TokBegin
parseEnd          = verify TokEnd
parsePlus         = verify TokPlus
parseMinus        = verify TokMinus
parseSlash        = verify TokDiv
parseStar         = verify TokTimes
parseComma        = verify TokComma
parseLeftParent   = verify TokLeftPar
parseRightParent  = verify TokRightPar
parseEOF          = verify TokEOF
parseMinInt       = verify TokMinInt
parseMinDouble    = verify TokMinDouble
parseMaxInt       = verify TokMaxInt
parseMaxDouble    = verify TokMaxDouble
parseProgram      = verify TokProgram
parseLBracket     = verify TokLeftBracket
parseRBracket     = verify TokRightBracket
parseToInt        = verify TokToInt
parseToDouble     = verify TokToDouble
parseToChar       = verify TokToChar
parseLeftBracket  = verify TokLeftBracket
parseRightBracket = verify TokRightBracket
parseTokAbs       = verify TokAbs
parseTokSqrt      = verify TokSqrt
parseTokPower     = verify TokPower
parseAnd          = verify TokAnd
parseOr           = verify TokOr
parsePipe         = verify TokPipe
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
parseRef          = verify TokRef
parseRead         = verify TokRead
parseWith         = verify TokWith
parseWriteln      = verify TokWriteln
parseOf           = verify TokOf
parseTokArray     = verify TokArray
parseTokLeftPre   = verify TokLeftPre
parseTokRightPre  = verify TokRightPre
parseTokLeftPost  = verify TokLeftPost
parseTokRightPost = verify TokRightPost
parseTokExist     = verify TokExist
parseTokLeftPer   = verify TokLeftPercent
parseTokRightPer  = verify TokRightPercent
parseTokLeftBound = verify TokLeftBound
parseTokRightBound= verify TokRightBound
parseTokLeftA     = verify TokLeftA
parseTokRightA    = verify TokRightA
parseTokLeftInv   = verify TokLeftInv
parseTokRightInv  = verify TokRightInv
parseTokNot       = verify TokNot
parseTokEQ        = verify TokEQ
parseTokNE        = verify TokNE
parseTokLE        = verify TokLE
parseTokGE        = verify TokGE
parseTokLT        = verify TokLT
parseTokGT        = verify TokGT
parseTokImplies   = verify TokImplies
parseTokConse     = verify TokConsequent
parseTokMod       = verify TokMod
parseTokMax       = verify TokMax
parseTokMin       = verify TokMin
parseTokForall    = verify TokForall
parseTokNotExist  = verify TokNotExist
parseTokSigma     = verify TokSigma
parseTokPi        = verify TokPi


parseAnyToken :: MyParser Token
parseAnyToken = tokenPrim showTok updatePos testTok
                where
                  showTok (t, pos) = show t
                  testTok (t, pos) = Just t

parseTokID :: MyParser Token
parseTokID = tokenPrim showTok updatePos testTok
          where
            showTok (t, pos) = show t
            testTok (t, pos) = case t of
                                   TokId id  -> Just $ TokId id
                                   _ -> Nothing

parseID :: MyParser Text
parseID = tokenPrim showTok updatePos testTok
          where
            showTok (t, pos) = show t
            testTok (t, pos) = case t of
                                   TokId id  -> Just id
                                   _ -> Nothing

parseBool :: MyParser Bool
parseBool = tokenPrim showTok updatePos testTok
            where
              showTok (t, pos) = show t
              testTok (t, pos) = case t of
                                    TokBool b -> Just b
                                    _ -> Nothing

parseTokBool :: MyParser Token
parseTokBool = tokenPrim showTok updatePos testTok
            where
              showTok (t, pos) = show t
              testTok (t, pos) = case t of
                                    TokBool b -> Just $ TokBool b
                                    _ -> Nothing

parseType :: MyParser Type
parseType = tokenPrim showTok updatePos testTok
              where
                showTok (t, pos) = show t
                testTok (t, pos) = case t of
                                      TokType b -> Just b
                                      _ -> Nothing

parseChar :: MyParser Char
parseChar = tokenPrim showTok updatePos testTok
            where
              showTok (t, pos) = show t
              testTok (t, pos) = case t of
                                    TokChar b -> Just b
                                    _ -> Nothing

parseTokChar :: MyParser Token
parseTokChar = tokenPrim showTok updatePos testTok
            where
              showTok (t, pos) = show t
              testTok (t, pos) = case t of
                                    TokChar b -> Just $ TokChar b
                                    _ -> Nothing

parseString :: MyParser String
parseString = tokenPrim showTok updatePos testTok
                where
                  showTok (t, pos) = show t
                  testTok (t, pos) = case t of
                                      TokString b -> Just b
                                      _ -> Nothing

parseTokString :: MyParser Token
parseTokString = tokenPrim showTok updatePos testTok
                where
                  showTok (t, pos) = show t
                  testTok (t, pos) = case t of
                                       TokString b -> Just $ TokString b
                                       _ -> Nothing

number :: MyParser Integer
number = tokenPrim showTok updatePos testTok
         where
           showTok (t, pos)     = show t
           testTok (t, pos)     = case t of
                                    TokInteger n -> Just n
                                    _ -> Nothing

parseTokNumber :: MyParser Token
parseTokNumber = tokenPrim showTok updatePos testTok
         where
           showTok (t, pos) = show t
           testTok (t, pos) = case t of
                                TokInteger n -> Just $ TokInteger n
                                _ -> Nothing

parseDouble :: MyParser Double
parseDouble = tokenPrim showTok updatePos testTok
         where
           showTok (t, pos)     = show t
           testTok (t, pos)     = case t of
                                    TokFloat n -> Just n
                                    _ -> Nothing

parseTokDouble :: MyParser Token
parseTokDouble = tokenPrim showTok updatePos testTok
         where
           showTok (t, pos)     = show t
           testTok (t, pos)     = case t of
                                    TokFloat n -> Just $ TokFloat n
                                    _ -> Nothing