{-# LANGUAGE NamedFieldPuns #-}

module Parser.Type
    ( type'
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
                                    match, match', parens, putError)
-- import           Parser.Rhecovery
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
import           Text.Megaparsec   (between, getPosition, lookAhead,
                                    notFollowedBy, sepBy, try, (<|>))
--------------------------------------------------------------------------------

type' :: Parser Type
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
        GUndef -> GUndef
        t -> case msize of
          Nothing -> GUndef
          Just i -> GArray i t

    type'' = do
      -- If its not an array, then try with a basic type or a pointer
      from  <- getPosition
      tname <- identifier
      to    <- getPosition
      t     <- getType tname
      case t of
        Nothing -> do
          putError from (UndefinedType tname)
          pure GUndef
        Just t' -> isPointer t'

    isPointer :: Type -> Parser Type
    isPointer t = do
        match TokTimes
        isPointer (GPointer t)
      <|> pure t

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
          putError from (UndefinedType id)
          pure GUndef
        Just t' -> pure t'

arraySize :: Parser (Maybe Int32)
arraySize = do
  pos <- getPosition
  expr <- between (match TokLeftBracket) (match' TokRightBracket) expression
  case expr of
    Nothing ->
      pure Nothing
    Just Expression { expType, loc, exp' } -> case expType of
      GInt -> case exp' of
        Value (IntV i) -> return (Just i)
        Value _ -> error "internal error: Type and Value mismatch"
        _       -> do
          putError pos . UnknownError $
            "El tamaño de una variable debe ser una constante, y no puede \
            \incluir cuantificaciones."
          pure Nothing
      _ -> do
        putError pos . UnknownError $
          "El tamaño de una variable debe ser una constante de tipo entero, \
          \sin cuantificaciones."
        pure Nothing

abstractType :: Parser Type
abstractType
   =  type'
  <|> do {match TokSet;      match TokOf; GSet      <$> typeVar }
  <|> do {match TokMultiset; match TokOf; GMultiset <$> typeVar }
  <|> do {match TokSeq;      match TokOf; GSeq      <$> typeVar }

  <|> do {match TokFunc; ba <- typeVar; match TokArrow;   bb <- typeVar; pure $ GFunc ba bb }
  <|> do {match TokRel;  ba <- typeVar; match TokBiArrow; bb <- typeVar; pure $ GRel  ba bb }

  <|> (GTuple <$> parens (typeVar `sepBy` match TokComma))

  <|> typeVar

typeVar :: Parser Type
typeVar = do
  name <- identifier
  notFollowedBy (match TokTimes)
  pure $ GTypeVar name -- polymorphism
