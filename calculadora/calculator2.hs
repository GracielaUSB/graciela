module Parser where
  
import Text.Parsec.Error
import Text.ParserCombinators.Parsec
import qualified Control.Applicative as AP
 
data Token =   TokPlus 
             | TokMinus 
             | TokStar 
             | TokSlash 
             | TokEnd 
             | TokComma 
             | TokLeftParent 
             | TokRightParent 
             | TokInteger String
             | TokError
      deriving (Show, Read)

lexer :: Parser (Token)
lexer = spaces >> (      (char '+' >> spaces >> return TokPlus)
                     <|> (char '-' >> spaces >> return TokMinus)
                     <|> (char '*' >> spaces >> return TokStar)
                     <|> (char '/' >> spaces >> return TokSlash)
                     <|> (char ',' >> spaces >> return TokComma)
                     <|> (char '(' >> spaces >> return TokLeftParent)
                     <|> (char ')' >> spaces >> return TokRightParent)
                     <|> (char '$' >> spaces >> return TokEnd)
                     <|> ((many1 digit) AP.<* spaces >>= return . (TokInteger))
                     <|> (anyChar >> return TokError))

expr :: Parser (Either [ParseError] Integer)
expr = do t <- term
          do (lookAhead(char '$' <|> char ',' <|> char ')') >> return t)
              <|> do tok <- lexer
                     case tok of
                       { TokPlus    -> expr >>= return . (verifyBinError (+) t)
                       ; TokMinus   -> expr >>= return . (verifyBinError (-) t)
                       ; TokError   -> (genNewError (panicMode lookAheadSet) "operador" tok >>= return . (checkError t))
                       ; otherwise  -> (genNewError (panicMode lookAheadSetList) "fin de archivo o coma" tok >>= return . (checkError t))
                       }

term = do p <- factor
          do (lookAhead(char '+' <|> char '-' <|> char '$' <|> char ')' <|> char ',') >> return p)
              <|> do tok <- lexer
                     case tok of
                       { TokSlash   -> term >>= return . (verifyBinError (div) p)
                       ; TokStar    -> term >>= return . (verifyBinError (*)   p)
                       ; TokError   -> (genNewError (panicMode lookAheadSet) "operador" tok >>= return . (checkError p))
                       ; otherwise  -> (genNewError (panicMode lookAheadSetList) "fin de archivo, operador o coma" tok >>= return . (checkError p))
                       }

factor = do tok <- lexer
            case tok of
              { TokLeftParent  -> do e     <- expr 
                                     tok'  <- lexer
                                     case tok' of
                                       { TokRightParent -> return e                                     
                                       ; TokError       ->  (genNewError (panicMode lookAheadSet) ")" tok' >>= return . Left . return)
                                       }
              ; TokInteger int -> return (Right (read int))
              ; TokError       -> (genNewError (panicMode lookAheadSet) "numero" tok >>= return . Left . return)
              }
     
genNewError pm msg e = do pos  <- getPosition
                          pm
                          return (newErrorMessage (Message ("Esperaba un " ++ msg ++ " en vez de " ++ (show e))) pos)

parseListExpr :: Parser (Either [ParseError] [Integer])
parseListExpr = do  spaces
                    e <- expr
                    do tok <- lexer
                       case tok of
                        { TokComma  -> parseListExpr >>= return . (verifyBinError (:) e)
                        ; TokEnd    -> return (fmap (AP.pure) e)
                        ; otherwise -> do err <- genNewError (panicMode lookAheadSetList) "fin de archivo, operador o coma" tok
                                          tok' <- lexer
                                          case tok' of
                                          { TokComma -> do res <- parseListExpr
                                                           return (verifyBinError (:) (checkError e err) res)
                                          ; TokEnd   -> return (checkError e err)
                                          }
                        }

checkError (Left xs) s = Left (s:xs)
checkError _         s = Left [s] 

verifyBinError _  (Left xs) (Left ys) = Left (xs ++ ys)
verifyBinError _  (Left xs) _         = Left xs
verifyBinError _  _         (Left ys) = Left ys
verifyBinError op (Right x) (Right y) = Right(op x y) 

lookAheadSet = lookAhead(char '-' <|> char '*' <|> char '/' <|> char '+' <|> char '$' <|> char ',' <|> char '(' <|> char ')')

lookAheadSetList = lookAhead(char ',' <|> char '$')

panicMode until = manyTill anyChar until 

play parser inp = case runParser (parser) () "" (inp ++ "$") of
                    { Left err -> print err
                    ; Right ans -> print ans
                    }
