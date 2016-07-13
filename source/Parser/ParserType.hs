module Parser.ParserType
    ( myBasicType
    , parsePointer
    , myType
    , parseConstNumber
    ) where
--------------------------------------------------------------------------------
import           Location
import           ParserState
import           Graciela
import           Token
import           Parser.TokenParser
import           Type
--------------------------------------------------------------------------------
import           Data.Text   (Text)
import           Text.Parsec
--------------------------------------------------------------------------------

myBasicType :: Graciela Token -> Graciela Token -> Graciela Type
myBasicType follow recSet = parseType

parsePointer :: Type -> Graciela Type
parsePointer t =
  do
    parseStar
    parsePointer $GPointer t
  <|> return t

myType :: Graciela Token -> Graciela Token -> Graciela Type
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

       <|> do id <- parseId
              parseOf
              t <- myType follow recSet
              -- lookup (id,t) y devuelve si es un tipo abstracto o uno concreto
              return (GDataType id [t] [] [])


parseConstNumber :: Graciela Token -> Graciela Token
                  -> Graciela (Maybe (Either Text Integer))
parseConstNumber follow recSet =
    do pos <- getPosition
       do  lookAhead follow
           genNewEmptyError
           return Nothing
           <|> do e <- number
                  return $ return $ return e
           <|> do id <- parseId
                  res <- lookUpConstIntParser id (toLocation pos)
                  case res of
                    Nothing -> return Nothing
                    Just _  -> return $ return $ Left id
