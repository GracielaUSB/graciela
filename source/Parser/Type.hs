{-# LANGUAGE NamedFieldPuns #-}

module Parser.Type
    ( basicType
    , type'
    , abstractType
    , typeVarDeclaration
    , typeVar
    ) where
--------------------------------------------------------------------------------
import           AST.Declaration
import           AST.Definition    (Definition(..))
import           AST.Expression    (Expression (..), Expression' (Value),
                                    Value (..))
import           AST.Struct
import           Entry
import           Error
import           Location
import           Parser.Expression (expression)
import           Parser.Monad      (Parser, getType, identifier, integerLit,
                                    match, parens, putError, getStruct,
                                    unsafeGenCustomError)
import           Parser.State
import           Parser.Recovery
import           SymbolTable       (lookup)
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Lens      (use, (%=))
import           Control.Monad     (void, when)
import           Data.Int          (Int32)
import           Data.Map          as Map (insert, elems, lookup, null)
import           Data.List         (intercalate)
import           Data.Monoid       ((<>))
import           Data.Text         (Text, unpack)
import           Prelude           hiding (lookup)
import           Text.Megaparsec   (getPosition, lookAhead, notFollowedBy,
                                    sepBy, try, (<|>), optional)
import         Debug.Trace
--------------------------------------------------------------------------------

basicType :: Parser Type
basicType = do 
  from <- getPosition
  t <- type'
  if t =:= GOneOf [GBool, GChar, GInt, GFloat]
    then pure t
    else do 
      putError from $ UnknownError $ show t <> " is not a basic type"
      pure GUndef



type' :: Parser Type
type' = parenType <|> try userDefined<|> try arrayOf <|> try type''
  where
    parenType = do 
      t <- parens type'
      isPointer t
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
    -- TODO: Or if its a user defined Type
    userDefined = do
      from <- getPosition
      id <- lookAhead identifier
      t  <- getStruct id

      case t of
        Nothing -> do
          current <- use currentStruct 
          case current of 
            Just (name, _, t) -> if name == id 
                then do
                  identifier
                  return $ GDataType name t
                else do
                  notFollowedBy identifier
                  return GUndef
            Nothing -> do
              notFollowedBy identifier
              return GUndef

        Just ast@Struct {structName, structTypes, structDecls, structProcs} -> do
          ok <- checkType structName structTypes from
          full <- use fullDataTypes
          
          if ok
            then do
              case structName `Map.lookup` full of
                Nothing -> do
                  fullDataTypes %= Map.insert structName ast
                  
                  -- mapM (\x -> definitions %= insert (defName x) x) structProcs
                  return $ GDataType structName structTypes
                Just x -> return $ GDataType structName structTypes
            else return GUndef
    
    checkType name types from = do
      identifier
      polymorphism <- return . concat =<< 
          (optional . parens $ type' `sepBy` match TokComma)

      let
        plen = length polymorphism
        slen = length types
        show' []  = ""
        show'  l  = "(" <> intercalate "," (fmap show l) <> ")"
     
      if slen == plen 
        then return True

      else if slen == 0
        then do 
          putError from $ UnknownError $ "Type `" <> unpack name <> 
              "` does not expect " <> show' polymorphism <> " as argument"
          return False

      else if slen > plen
        then do 
          putError from $ UnknownError $ "Type `" <> unpack name <> 
             "` expected " <> show slen <> " types " <> show' types <>
             "\n\tbut recived " <> show plen <> " " <> show' polymorphism
          return False

      else do
          putError from $ UnknownError $ "Type `" <> unpack name <> 
               "` expected only " <> show slen <> " types as arguments " <> 
               show' types <> "\n\tbut recived " <> show plen <> " " <>
               show' polymorphism
          return False

isPointer :: Type -> Parser Type
isPointer t = do
    match TokTimes
    isPointer (GPointer t)
  <|> pure t         

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


typeVarDeclaration  :: Parser Type
typeVarDeclaration = do 
  tname <- lookAhead identifier
  t     <- getType tname
  case t of
    Nothing -> do
      identifier
      notFollowedBy (match TokTimes)
      typesVars %= (tname:)
      return $ GTypeVar tname
    Just _ -> do 
      notFollowedBy identifier
      return $ GUndef

typeVar :: Parser Type
typeVar = do
  tname <- lookAhead identifier
  tvars <- use typesVars
  if tname `elem` tvars
    then do 
      identifier
      isPointer $ GTypeVar tname
    else do 
      notFollowedBy identifier
      return $ GUndef

