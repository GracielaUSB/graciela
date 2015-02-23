module Parser where
  
import Text.Parsec
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import qualified Control.Applicative as AP
import qualified Data.Text as T
import qualified Data.Text.Read as TR

playLexer :: Parsec T.Text () [TokenPos] -> T.Text -> IO ()
playLexer parser inp = case runParser (parser) () "" (T.snoc inp '$') of
                       { Left err -> print err
                       ; Right ans -> print ans
                       }
                       
playParser :: Parsec [TokenPos] () (Either [ParseError] Integer) -> Either ParseError [TokenPos] -> IO ()
playParser parser (Right l) = case runParser (parser) () "" (l) of
                                { Left err -> print err
                                ; Right ans -> print ans
                                }
playParser _ (Left _) = putStrLn "LALALALALALALLALALALAL"

data Token =   TokPlus
             | TokMinus 
             | TokStar 
             | TokSlash 
             | TokEnd 
             | TokComma 
             | TokLeftParent 
             | TokRightParent 
             | TokInteger Integer
             | TokError
      deriving (Show, Read, Eq)

type TokenPos = (Token, SourcePos)

lexer :: Parsec T.Text () (Token)
lexer = spaces >> (      (char '+' >> spaces >> return TokPlus)
                     <|> (char '-' >> spaces >> return TokMinus)
                     <|> (char '*' >> spaces >> return TokStar)
                     <|> (char '/' >> spaces >> return TokSlash)
                     <|> (char ',' >> spaces >> return TokComma)
                     <|> (char '(' >> spaces >> return TokLeftParent)
                     <|> (char ')' >> spaces >> return TokRightParent)
                     <|> (char '$' >> spaces >> return TokEnd)
                     <|> ((many1 digit) AP.<* spaces >>= return . (TokInteger . read))
                     <|> (anyChar >> return TokError))

lexerList = do  pos <- getPosition
                tok <- lexer
                case tok of
                  { TokEnd     -> return [(tok, pos)]
                  ; otherwise  -> lexerList >>= return . ((:) (tok, pos))
                  }

makeTokenParser x = token showTok posTok testTok
                    where
                      showTok (t, pos) = show t
                      posTok  (t, pos) = pos
                      testTok (t, pos) = if x == t then Just () else Nothing

verify :: Token -> Parsec ([TokenPos]) () ()
verify token = makeTokenParser token

parsePlus = verify TokPlus
parseMinus = verify TokMinus
parseSlash = verify TokSlash
parseStar = verify TokStar
parseComma = verify TokComma
parseLeftParent = verify TokLeftParent
parseRightParent = verify TokRightParent
parseError = verify TokError
parseEnd  = verify TokEnd
parseAnyToken = token showTok posTok testTok
                where
                  showTok (t, pos) = show t
                  posTok  (t, pos) = pos
                  testTok (t, pos) = Just (t)

number :: Parsec ([TokenPos]) () (Integer)
number = token showTok posTok testTok
          where
            showTok (t, pos) = show t
            posTok  (t, pos) = pos
            testTok (t, pos) = case t of
                                { TokInteger n -> Just n
                                ; otherwise    -> Nothing
                                }

expr' :: Parsec [TokenPos] () (Either [ParseError] Integer)
expr' = do t <- term'
           do (lookAhead(parseEnd <|> parseRightParent) >> return t)
              <|> (parsePlus  AP.*> expr' >>= return . (verifyBinError (+) t))
              <|> (parseMinus AP.*> expr' >>= return . (verifyBinError (-) t))
              <|> (genNewError (lookAheadSet) ("+ o -") >>= return . (checkError t))
  
checkError (Left xs) s = Left (s:xs)
checkError _         s = Left [s] 

            -- 
            --   <|> do tok <- lexer
            --          case tok of
            --            { TokPlus    -> expr >>= return . (verifyBinError (+) t)
            --            ; TokMinus   -> expr >>= return . (verifyBinError (-) t)
            --            ; TokError   -> (genNewError (panicMode lookAheadSet) "operador" tok >>= return . (checkError t))
            --            ; otherwise  -> (genNewError (panicMode lookAheadSetList) "fin de archivo o coma" tok >>= return . (checkError t))
            --            }

term' :: Parsec [TokenPos] () (Either [ParseError] Integer)
term' = do p <- factor
           do (lookAhead(parsePlus <|> parseMinus <|> parseEnd <|> parseRightParent) >> return p)
              <|> (parseSlash AP.*> term' >>= return . (verifyBinError (div) p))
              <|> (parseStar  AP.*> term' >>= return . (verifyBinError (*)   p))
              <|> (genNewError (lookAheadSet) ("* o /") >>= return . (checkError p))
              <?> "termino"
               -- <|> do tok <- lexer
               --        case tok of
               --          { TokSlash   -> term >>= return . (verifyBinError (div) p)
               --          ; TokStar    -> term >>= return . (verifyBinError (*)   p)
               --          ; TokError   -> (genNewError (panicMode lookAheadSet) "operador" tok >>= return . (checkError p))
               --          ; otherwise  -> (genNewError (panicMode lookAheadSetList) "fin de archivo, operador o coma" tok >>= return . (checkError p))
               --          }

panicMode until = manyTill parseAnyToken until 

factor :: Parsec [TokenPos] () (Either [ParseError] Integer)
factor = do try(parseLeftParent AP.*> expr' AP.<* parseRightParent >>= return)
            <|> (number >>= return . Right)
            <?> "numeros"
--            case tok of
--              { TokLeftParent  -> do e     <- expr 
--                                     tok'  <- lexer
--                                     case tok' of
--                                       { TokRightParent -> return e                                     
--                                       ; TokError       ->  (genNewError (panicMode lookAheadSet) ")" tok' >>= return . Left . return)
--                                       }
--              ; TokInteger int -> return (Right int)
--              ; TokError       -> (genNewError (panicMode lookAheadSet) "numero" tok >>= return . Left . return)
--              }

getNumber (Right (number, _)) = number
getNumber _                   = 0
 
genNewError :: Parsec [TokenPos] () () -> String -> Parsec [TokenPos] () (ParseError)
genNewError laset msg = do  pos  <- getPosition
                            e <- parseAnyToken
                            panicMode laset
                            return (newErrorMessage (Message ("Esperaba un " ++ msg ++ " en vez de " ++ (show e))) pos)

--parseListExpr :: Parsec T.Text () (Either [ParseError] [Integer])
--parseListExpr = do  spaces
--                    e <- expr
--                    do tok <- lexer
--                       case tok of
--                        { TokComma  -> parseListExpr >>= return . (verifyBinError (:) e)
--                        ; TokEnd    -> return (fmap (AP.pure) e)
--                        ; otherwise -> do err <- genNewError (panicMode lookAheadSetList) "fin de archivo, operador o coma" tok
--                                          tok' <- lexer
--                                          case tok' of
--                                          { TokComma -> do res <- parseListExpr
--                                                           return (verifyBinError (:) (checkError e err) res)
--                                          ; TokEnd   -> return (checkError e err)
--                                          }
--                        }


verifyBinError _  (Left xs) (Left ys) = Left (xs ++ ys)
verifyBinError _  (Left xs) _         = Left xs
verifyBinError _  _         (Left ys) = Left ys
verifyBinError op (Right x) (Right y) = Right(op x y) 

lookAheadSet = lookAhead( parseMinus 
                      <|> parseStar 
                      <|> parseSlash
                      <|> parsePlus 
                      <|> parseEnd 
                      <|> parseComma 
                      <|> parseLeftParent
                      <|> parseRightParent)
--
--lookAheadSetList = lookAhead(char ',' <|> char '$')


play :: Parsec T.Text () (Either [ParseError] Integer) -> T.Text -> IO ()
play parser inp = case runParser (parser) () "" (T.snoc inp '$') of
                    { Left err -> print err
                    ; Right ans -> print ans
                    }
