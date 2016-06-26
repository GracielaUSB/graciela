module Parser.TokenParser where
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
                            then Just t 
                            else Nothing

updatePos :: SourcePos -> (Token, SourcePos) -> [TokenPos] -> SourcePos
updatePos _ _ ((_, pos):xs) = pos
updatePos _ (_, pos) []     = pos

parseAbort         = verify TokAbort
parseAnd           = verify TokAnd
parseArrow         = verify TokArrow
parseAssign        = verify TokAsig
parseBegin         = verify TokBegin
parseColon         = verify TokColon
parseComma         = verify TokComma
parseConst         = verify TokConst
parseDo            = verify TokDo
parseEnd           = verify TokEnd
parseEOF           = verify TokEOF
parseFi            = verify TokFi
parseFunc          = verify TokFunc
parseIf            = verify TokIf
parseIn            = verify TokIn
parseInOut         = verify TokInOut
parseLBracket      = verify TokLeftBracket
parseLeftBracket   = verify TokLeftBracket
parseLeftParent    = verify TokLeftPar 
parseMaxDouble     = verify TokMaxDouble
parseMaxInt        = verify TokMaxInt
parseMinDouble     = verify TokMinDouble
parseMinInt        = verify TokMinInt
parseMinus         = verify TokMinus
parseOd            = verify TokOd
parseOf            = verify TokOf
parseOr            = verify TokOr
parseOut           = verify TokOut
parsePipe          = verify TokPipe
parsePlus          = verify TokPlus
parseProc          = verify TokProc
parseProgram       = verify TokProgram
parseRandom        = verify TokRandom
parseRBracket      = verify TokRightBracket
parseRead          = verify TokRead
parseRef           = verify TokRef
parseRightBracket  = verify TokRightBracket
parseRightParent   = verify TokRightPar
parseSemicolon     = verify TokSemicolon
parseSepGuards     = verify TokSepGuards
parseSkip          = verify TokSkip
parseSlash         = verify TokDiv
parseStar          = verify TokTimes
parseToChar        = verify TokToChar
parseToDouble      = verify TokToDouble
parseToInt         = verify TokToInt
parseTokAbs        = verify TokAbs
parseTokArray      = verify TokArray
parseTokCloseBlock = verify TokCloseBlock
parseTokConse      = verify TokConsequent
parseTokEQ         = verify TokEQ
parseTokExist      = verify TokExist
parseTokForall     = verify TokForall
parseTokGE         = verify TokGE
parseTokGT         = verify TokGT
parseTokImplies    = verify TokImplies
parseTokLE         = verify TokLE
parseTokLeftA      = verify TokLeftA
parseTokLeftBound  = verify TokLeftBound
parseTokLeftInv    = verify TokLeftInv
parseTokLeftPer    = verify TokLeftPercent
parseTokLeftPost   = verify TokLeftPost
parseTokLeftPre    = verify TokLeftPre
parseTokLT         = verify TokLT
parseTokMax        = verify TokMax
parseTokMin        = verify TokMin
parseTokMod        = verify TokMod
parseTokNE         = verify TokNE
parseTokNot        = verify TokNot
parseTokNotExist   = verify TokNotExist
parseTokOpenBlock  = verify TokOpenBlock
parseTokPi         = verify TokPi
parseTokPower      = verify TokPower
parseTokRightA     = verify TokRightA
parseTokRightBound = verify TokRightBound
parseTokRightInv   = verify TokRightInv
parseTokRightPer   = verify TokRightPercent
parseTokRightPost  = verify TokRightPost
parseTokRightPre   = verify TokRightPre
parseTokSigma      = verify TokSigma
parseTokSqrt       = verify TokSqrt
parseVar           = verify TokVar
parseWith          = verify TokWith
parseWrite         = verify TokWrite
parseWriteln       = verify TokWriteln


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