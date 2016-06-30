module Parser.ParserType 
    ( myBasicType
    , parsePointer
    , myType
    , parseConstNumber
    ) where
--------------------------------------------------------------------------------
import           Location
import           ParserState
import           State
import           Token
import           Parser.TokenParser
import           Type
--------------------------------------------------------------------------------
import           Data.Text   (Text)
import           Text.Parsec
--------------------------------------------------------------------------------

myBasicType :: MyParser Token -> MyParser Token -> MyParser Type
myBasicType follow recSet = parseType

parsePointer :: Type -> MyParser Type
parsePointer t = 
  do
    parseStar
    parsePointer $GPointer t
  <|> return t

myType :: MyParser Token -> MyParser Token -> MyParser Type
myType follow recSet = 
      do t <- myBasicType follow recSet
         try $do parsePointer t
          <|> return t

       <|> do parseTokArray
              parseLeftBracket
              n <- parseConstNumber parseOf (recSet <|> parseOf)
              parseRightBracket
              parseOf
              t <- myType follow recSet
              case n of
                  Nothing -> return GEmpty
                  Just n' -> return $ GArray n' t

       <|> do id <- parseID
              parseOf
              t <- myType follow recSet
              -- lookup (id,t) y devuelve si es un tipo abstracto o uno concreto
              return (GDataType id [t] [] [])


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
