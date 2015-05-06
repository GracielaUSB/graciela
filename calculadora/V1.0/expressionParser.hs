-- Parser basico para una lista de expresiones separadas por coma
-- Autores:
--  Joel Araujo
--  Jose Jimenez

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Error
import Control.Monad.Identity (Identity)
import Control.Arrow
 
expr  = buildExpressionParser table term
      <?> "expression"


lexer :: P.TokenParser ()
lexer =   P.makeTokenParser 
          ( emptyDef
          { P.reservedOpNames = ["*", "/", "+", "-"] }
          )
                            

reservedOp  = P.reservedOp lexer
parens      = P.parens lexer
natural     = P.natural lexer

term =  natural
        <|> parens expr
        <?> "expression"

table   = [
            [  
              Infix (do{reservedOp "*"; return (*)    } <?> "operator") AssocLeft, 
              Infix (do{reservedOp "/"; return (div)  } <?> "operator") AssocLeft
            ],
            [
              Infix (do{reservedOp "+"; return (+)    } <?> "operator") AssocLeft,
              Infix (do{reservedOp "-"; return (-)    } <?> "operator") AssocLeft
            ]
          ]

exprError pos = newErrorMessage (Message ("Error: Esperaba un numero o operador en vez de ")) pos

parseListExp :: ParsecT String () Identity (Either [Integer] [(ParseError, Char)])
parseListExp = do { e <- expr
                  ;   do { char ','; spaces;  xs <- parseListExp; return (left(e:) xs) }
                 <|>  do { eof; return(Left[e]) }
                 <|>  do {  pos <- getPosition
                         ;  ys <- try(manyTill anyChar (char ','))
                         ;  spaces
                         ;  xs <- parseListExp
                         ; return ( if isLeft xs then Right([(exprError pos, head ys)])
                                    else fmap (((exprError pos, (head ys))):) xs
                                   ) 
                         }
                 <|>  do {  pos <- getPosition
                         ;  ys  <- many anyChar 
                         ;  return (Right[(exprError pos, head ys)])  
                         }
                  }

isLeft (Left _) = True
isLeft _        = False

printErrors :: [(ParseError, Char)] -> IO ()
printErrors xs = mapM_ putStrLn (map (\(p, c) -> (show p) ++ [c]) xs)

play :: String -> IO ()
play inp = case runParser parseListExp () "" inp of
             { Left err -> print err
             ; Right ans -> case ans of 
                              { Left  xs -> print xs
                              ; Right xs -> printErrors xs
                              }
             }
