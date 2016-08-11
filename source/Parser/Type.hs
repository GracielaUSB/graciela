{-# LANGUAGE NamedFieldPuns #-}

module Parser.Type
    ( basicType
    , type'
    , abstractType
    , typeVar
    ) where
--------------------------------------------------------------------------------
import           AST.Expression (Expression(..))
import           AST.Type
import           Entry
import           Graciela
import           Parser.Expression (expression)
import           Parser.Recovery
import           Parser.Token      (identifier, integerLit, match, parens)
import           SymbolTable       (lookup)
import           Token
--------------------------------------------------------------------------------
import           Control.Lens      (use)
import           Control.Monad     (void, when)
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
  if t == Nothing 
    then do
      genCustomError ("El tipo `" <> unpack tname <> "` no existe.")
      return GUndef

  else do
    let (Just t') = t
    if t' =:= GOneOf [GInt, GFloat, GBool, GChar]
      then return t'
      else do
        genCustomError ("El tipo `" <> unpack tname <> "` no es un tipo basico.")
        return GUndef


type' :: Graciela Type
type' = try arrayOf <|> try type'' <|> userDefined
  where
    -- Try to parse an array type
    arrayOf = do
        match TokArray
        match TokLeftBracket
        expr <- arraySize
        match TokRightBracket
        match TokOf
        t <- type'
        if t == GUndef
          then return t
          else return $ GArray expr t
    type'' = do
        -- If its not an array, then try with a basic type or a pointer
        tname <- identifier
        t  <- getType tname
        when (t == Nothing) $ void $genCustomError ("El tipo `"<>unpack tname<>"` no existe.")
        let (Just t') = t
        t'' <- isPointer t'
        return t''
    isPointer :: Type -> Graciela Type
    isPointer t = do
        match TokTimes
        isPointer (GPointer t)
      <|> return t

    -- Or ty if its a user defined Type
    userDefined = do
      id <- identifier
      match TokOf
      t <- type'
      return (GDataType id)

arraySize :: Graciela Expression
arraySize = do
  pos <- getPosition
  expr <- safeExpression
  case expr of
    BadExpression { loc } -> do 
      return expr
    Expression { constant } | constant == True -> return expr
    Expression { loc } -> do
      genCustomError "El tamaño del arreglo debe definirse usando un numero o una variable constante"
      return $ BadExpression loc

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

