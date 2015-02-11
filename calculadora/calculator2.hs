module Parser where
  
import Text.Parsec.Error
import Text.ParserCombinators.Parsec
import qualified Control.Applicative as AP
 
expr :: Parser (Either [ParseError] Integer)
expr = do t <- term
          do try (cleanSpaces(char '+')) >> expr >>= return . (verifyBinError (+) t)
             <|> (try (cleanSpaces(char '-')) >> expr >>= return . (verifyBinError (-) t))
             <|> (lookAhead(char '$' <|> char ',') >> return t)
             <|> (genNewError panicMode "operador" >>= return . (checkError t))

term = do p <- factor
          do try (cleanSpaces(char '*')) >> term >>= return . (verifyBinError (*) p)
             <|> (try (cleanSpaces(char '/')) >> term >>=  return . (verifyBinError (div) p))
             <|> (lookAhead(char '+' <|> char '-' <|> char '$' <|> char ',') >> return p)
             <|> (genNewError panicMode "operador" >>= return . (checkError p))

factor = do try(cleanSpaces(char '('))
            e <- expr
            do try(cleanSpaces(char ')')) >> return e
               <|> (genNewError panicMode ")" >>= return . Left . return)
      <|> number
  
number = do ds <- cleanSpaces(many1 digit)
            return (Right (read ds))
      <|> (genNewError panicMode "numero" >>= return . Left . return)

genNewError pm msg = do pos  <- getPosition
                        ys   <- getInput
                        pm
                        return (newErrorMessage (Message ("Esperaba un " ++ msg ++ " en vez de " ++ [head ys])) pos)

parseListExpr :: Parser (Either [ParseError] [Integer])
parseListExpr = do  spaces
                    e <- expr
                    do try (cleanSpaces(char ',')) >> parseListExpr >>= return . (verifyBinError (:) e)
                       <|> (try(char '$') >> return (fmap (AP.pure) e))

checkError (Left xs) s = Left (s:xs)
checkError _         s = Left [s] 

verifyBinError _  (Left xs) (Left ys) = Left (xs ++ ys)
verifyBinError _  (Left xs) _         = Left xs
verifyBinError _  _         (Left ys) = Left ys
verifyBinError op (Right x) (Right y) = Right(op x y) 

panicMode = manyTill anyChar (lookAhead(char '-' <|> char '*' <|> char '/' <|> char '+' <|> char '$' <|> char ','))


cleanSpaces p = do  r <- p
                    spaces
                    return r

play :: String -> IO ()
play inp = case runParser parseListExpr () "" (inp ++ "$") of
             { Left err -> print err
             ; Right ans -> print ans
             }
