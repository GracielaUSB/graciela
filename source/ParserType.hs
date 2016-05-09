module ParserType where
--------------------------------------------------------------------------------
import           Location
import           ParserState
import           State
import           Token
import           TokenParser
import           Type
--------------------------------------------------------------------------------
import           Data.Text   (Text)
import           Text.Parsec
--------------------------------------------------------------------------------

myBasicType :: MyParser Token -> MyParser Token -> MyParser Type
myBasicType follow recSet = parseType


myType :: MyParser Token -> MyParser Token -> MyParser Type
myType follow recSet =
    myBasicType follow recSet
       <|> do parseTokArray
              parseLeftBracket
              n <- parseConstNumber parseOf (recSet <|> parseOf)
              parseRightBracket
              parseOf
              t <- myType follow recSet

              case n of
                  Nothing -> return GEmpty
                  Just n' -> return $ GArray n' t


parseConstNumber :: MyParser Token -> MyParser Token
                  -> MyParser (Maybe (Either Text Integer))
parseConstNumber follow recSet =
    do pos <- getPosition
       do  lookAhead follow
           genNewEmptyError
           return Nothing
           <|> do e <- number
                  return $ return $ return e
           <|> do id <- parseID
                  res <- lookUpConstIntParser id (toLocation pos)
                  case res of
                    Nothing -> return Nothing
                    Just _  -> return $ return $ Left id
