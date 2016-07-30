module Parser.Type
    ( myBasicType
    , parsePointer
    , myType
    , parseConstNumber
    ) where
--------------------------------------------------------------------------------
import           Graciela
import           Parser.Token    (identifier, integerLit, match)
import           ParserState
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Monad   (void, when)
import           Data.Text       (Text, unpack)
import           Text.Megaparsec (getPosition, lookAhead, try, (<|>))
import           Text.Megaparsec hiding (Token)
--------------------------------------------------------------------------------

myBasicType :: Graciela Token -> Graciela Text
myBasicType follow = identifier

parsePointer :: Type -> Graciela Type
parsePointer t =
  do
    match TokTimes
    parsePointer $GPointer t
  <|> return t

myType :: Graciela Token -> Graciela Type
myType follow =
      do
        tname <- identifier
        t <- getType tname
        when (t == GError) $ void $genCustomError ("Tipo de variable `"++unpack tname++"` no existe.")
        parsePointer t

      <|> do  
            match TokArray
            match TokLeftBracket
            n <- parseConstNumber (match TokOf)
            match TokRightBracket
            match TokOf
            t <- myType follow
            case n of
                Nothing -> return GEmpty
                Just n' -> return $ GArray n' t

      <|> do
            id <- identifier
            match TokOf
            t <- myType follow
            -- lookup (id,t) y devuelve si es un tipo abstracto o uno concreto
            return (GDataType id [t] [] [])


parseConstNumber :: Graciela Token -> Graciela (Maybe (Either Text Integer))
parseConstNumber follow =
    do pos <- getPosition
       do  lookAhead follow
           genNewEmptyError
           return Nothing
           <|> do e <- integerLit
                  return $ return $ return e
           <|> do id <- identifier
                  res <- lookUpConstIntParser id pos
                  case res of
                    Nothing -> return Nothing
                    Just _  -> return $ return $ Left id
