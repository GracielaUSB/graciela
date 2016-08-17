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
import           Graciela
import           Location
import           Parser.Expression (expression)
import           Parser.Recovery
import           Parser.Token      (identifier, integerLit, match, parens)
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

basicType :: Graciela Type
basicType = do
  tname <- identifier
  t <- getType tname

  case t of
    Nothing -> do
      unsafeGenCustomError ("El tipo `" <> unpack tname <> "` no existe.")
      return GUndef
    Just t' ->
      if t' =:= GOneOf [GInt, GFloat, GBool, GChar]
        then return t'
        else do
          unsafeGenCustomError $
            "El tipo `" <> unpack tname <> "` no es un tipo basico."
          return GUndef


type' :: Graciela Type
type' = try arrayOf <|> try type'' <|> userDefined
  where
    -- Try to parse an array type
    arrayOf = do
      match TokArray
      match TokLeftBracket
      msize <- arraySize
      match TokRightBracket
      match TokOf
      t <- type'
      pure $ if t == GUndef
        then t
        else case msize of
          Just i  -> GArray i t
          Nothing -> GUndef
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
          return GUndef
        Just t' -> isPointer t'

    isPointer :: Type -> Graciela Type
    isPointer t = do
        match TokTimes
        isPointer (GPointer t)
      <|> return t

    -- TODO: Or if its a user defined Type
    userDefined = do
      id <- identifier
      match TokOf
      t <- type'
      return (GDataType id)

arraySize :: Graciela (Maybe Int32)
arraySize = do
  pos <- getPosition
  expr <- safeExpression
  case expr of
    BadExpression { loc } ->
      pure Nothing
    Expression { expType, loc, exp' } -> case expType of
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

abstractType :: Graciela Type
abstractType = type'
        <|> do {match TokSet;      match TokOf; GSet      <$> typeVar }
        <|> do {match TokMultiset; match TokOf; GMultiset <$> typeVar }
        <|> do {match TokSeq;      match TokOf; GSeq      <$> typeVar }
        <|> do {match TokFunc; ba <- typeVar; match TokArrow; bb <- typeVar; return $ GFunc ba bb}
        <|> do {match TokRel;  ba <- typeVar; match TokBiArrow; bb <- typeVar; return $ GRel ba bb}
        <|> GTuple <$> parens (typeVar `sepBy` match TokComma)
        <|> typeVar

typeVar :: Graciela Type
typeVar = do
  id <- identifier
  notFollowedBy (match TokTimes)
  return $ GTypeVar id -- polymorphism
