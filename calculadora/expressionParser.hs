import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim
import qualified Control.Applicative as AP
import Control.Monad.Identity (Identity)
import Control.Arrow
 
data Error = ParseError  { error::String
                    , numLinea::Int
                    , numColumna::Int
                    }
                    deriving(Show)

expr  = buildExpressionParser table term
      <?> "expression"


lexer :: P.TokenParser ()
lexer =   P.makeTokenParser 
          ( emptyDef
          { P.reservedOpNames = ["*", "/", "+", "-"]
          }
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

parseListExp :: ParsecT String () Identity (Either [ParseError] [Integer])
parseListExp = do { e <- expr
                  ;   do { char ','; spaces;  xs <- parseListExp; return (fmap(e:) xs)}
                 <|>  do { eof; return(Right[e]) }
                 <|>  do {  pos <- getPosition
                         ;  ys <- try(manyTill anyChar (char ','))
                         ;  spaces
                         ;  xs <- parseListExp
                         ; return (left((newErrorMessage (Message ("Error: Esperaba una '' en vez de '" ++ [head ys] ++ "'\n")) pos):) xs) 
                         }
                 <|>  do {  pos <- getPosition
                         ;  ys  <- many anyChar 
                         ;  return (Left[newErrorMessage(Message ("Error: Esperaba una '' en vez de '" ++ [head ys] ++ "'\n")) pos])  
                         }
                  }
     
play :: String -> IO ()
play inp = case runParser parseListExp () "" inp of
             { Left err -> do { print err }
             ; Right ans -> print ans
             }
