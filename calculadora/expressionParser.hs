import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim
 
expr    = buildExpressionParser table term
         <?> "expression"


lexer =   makeTokenParser haskellDef

term    =  do {ds <- many1 digit; spaces; notFollowedBy alphaNum; return (read ds)}
        <|> parens lexer expr
        <?> "simple expression"

table   = [
            [  
              Infix (do { char '*'; spaces; notFollowedBy letter; return (*)  }) AssocLeft, 
              Infix (do { char '/'; spaces; notFollowedBy letter; return (div)  }) AssocLeft
            ],
            [
              Infix (do { char '+'; spaces; notFollowedBy letter; return (+) }) AssocLeft,
              Infix (do { char '-'; spaces; notFollowedBy letter; return (-) }) AssocLeft
            ]
          ]

mySeparator :: Parsec String () ()
mySeparator = do
              spaces
              char ','
              spaces

parseListExp :: Parsec String () [Integer]
parseListExp = sepBy expr mySeparator
     
play :: String -> IO ()
play inp = case parse parseListExp "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }
