{-# LANGUAGE NamedFieldPuns #-}

module Parser.Type
    ( basicType
    , type'
    , abstractType
    , typeVar
    ) where
--------------------------------------------------------------------------------
import           AST.Expression    (Expression (..), Expression' (Value),
                                    Value (..))
import           Entry
import           Error
import           Location
import           Parser.Expression (expression)
import           Parser.Monad      (Parser, getType, identifier, integerLit,
                                    match, parens, putError,
                                    unsafeGenCustomError)
import           Parser.Recovery
import           SymbolTable       (lookup)
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Lens      (use)
import           Control.Monad     (void, when)
import           Data.Int          (Int32)
import           Data.Monoid       ((<>))
import           Data.Text         (Text, unpack)
import           Prelude           hiding (lookup)
import           Text.Megaparsec   (getPosition, lookAhead, notFollowedBy,
                                    sepBy, try, (<|>))
--------------------------------------------------------------------------------

basicType :: Parser (Maybe Type)
basicType = do
  tname <- identifier
  t <- getType tname

  case t of
    Nothing -> do
      unsafeGenCustomError ("El tipo `" <> unpack tname <> "` no existe.")
      pure . Just $ GUndef
    Just t' ->
      if t' =:= GOneOf [GInt, GFloat, GBool, GChar]
        then pure . Just $  t'
        else do
          unsafeGenCustomError $
            "El tipo `" <> unpack tname <> "` no es un tipo basico."
          pure . Just $ GUndef


type' :: Parser (Maybe Type)
type' = try userDefined <|> try arrayOf <|> try type''
  where
    -- Try to parse an array type
    arrayOf = do
      match TokArray
      match TokLeftBracket
      msize <- arraySize
      match TokRightBracket
      match TokOf
      t <- type'

      pure $ case t of
        Nothing -> Just GUndef
        Just GUndef -> Just GUndef
        Just t -> case msize of
          Nothing -> Just GUndef
          Just i -> Just $ GArray i t

    type'' = do
      -- If its not an array, then try with a basic type or a pointer
      from  <- getPosition
      tname <- identifier
      to    <- getPosition
      let loc = Location(from,to)
      t     <- getType tname
      case t of
        Nothing -> do
          putError loc (UndefinedType tname)
          pure . Just $ GUndef
        Just t' -> isPointer t'

    isPointer :: Type -> Parser (Maybe Type)
    isPointer t = do
        match TokTimes
        isPointer (GPointer t)
      <|> (pure . Just $ t)

    -- TODO: Or if its a user defined Type
    userDefined = do
      from <- getPosition
      id <- identifier
      match TokOf
      t     <- getType id
      polymorphism <- type'
      to <- getPosition
      case t of
        Nothing -> do
          putError (Location(from,to)) (UndefinedType id)
          pure . Just $ GUndef
        Just t' -> pure . Just $ t'

arraySize :: Parser (Maybe Int32)
arraySize = do
  pos <- getPosition
  expr <- safeExpression
  case expr of
    Nothing ->
      pure Nothing
    Just Expression { expType, loc, exp' } -> case expType of
      GInt -> case exp' of
        Value (IntV i) -> return (Just i)
        Value _ -> error "internal error: Type and Value mismatch"
        _       -> do
          unsafeGenCustomError
            "El tamaño de una variable debe ser una constante, y no puede \
            \incluir cuantificaciones."
          pure Nothing
      _ -> do
        unsafeGenCustomError
          "El tamaño de una variable debe ser una constante de tipo entero, \
          \sin cuantificaciones."
        pure Nothing

abstractType :: Parser (Maybe Type)
abstractType
   =  type'
  <|> do {match TokSet;      match TokOf; fmap GSet      <$> typeVar }
  <|> do {match TokMultiset; match TokOf; fmap GMultiset <$> typeVar }
  <|> do {match TokSeq;      match TokOf; fmap GSeq      <$> typeVar }

  <|> do {match TokFunc; ba <- typeVar; match TokArrow;   bb <- typeVar; pure $ GFunc <$> ba <*> bb }
  <|> do {match TokRel;  ba <- typeVar; match TokBiArrow; bb <- typeVar; pure $ GRel  <$> ba <*> bb }

  <|> (fmap GTuple . sequence <$> parens (typeVar `sepBy` match TokComma))

  <|> typeVar

typeVar :: Parser (Maybe Type)
typeVar = do
  id <- identifier
  notFollowedBy (match TokTimes)
  return . Just $ GTypeVar id -- polymorphism
