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
 
data Error = Error  { error::String
                    , numLinea::Int
                    , numColumna::Int
                    }

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

term =  Just AP.<$> natural
        <|> parens expr
        <?> "expression"

table   = [
            [  
              Infix (do{reservedOp "*"; return (AP.liftA2 (*))    } <?> "operator") AssocLeft, 
              Infix (do{reservedOp "/"; return (AP.liftA2 (div))  } <?> "operator") AssocLeft
            ],
            [
              Infix (do{reservedOp "+"; return (AP.liftA2 (+))    } <?> "operator") AssocLeft,
              Infix (do{reservedOp "-"; return (AP.liftA2 (-))    } <?> "operator") AssocLeft
            ]
          ]

parseListExp :: ParsecT String () Identity [Maybe Integer]
parseListExp = do { e <- expr
                  ;   do { char ','; spaces; xs <- parseListExp; return (e:xs) }
                 <|>  do { eof; return([e]) }
                 <|>  do { try(manyTill anyChar (char ',')); spaces; xs <- parseListExp; return(Nothing:xs) }
                 <|>  do { return ([Nothing]) }
                  }
     
play :: String -> IO ()
play inp = case runParser parseListExp () "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }
