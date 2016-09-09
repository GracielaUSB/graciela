{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Parser.Type
    ( basicType
    , type'
    , abstractType
    , typeVarDeclaration
    , typeVar
    ) where
--------------------------------------------------------------------------------
import           AST.Declaration
import           AST.Definition    (Definition(..),Definition'(..))
import           AST.Expression    (Expression (..), Expression' (Value),
                                    Value (..))
import           AST.Struct
import           Entry
import           Error
import           Location
import           Parser.Expression (expression)
import           Parser.Monad      (Parser, getType, identifier, integerLit,
                                    match, parens, putError, getStruct, match')
import           Parser.State
import           SymbolTable       (lookup)
import           Token
import           AST.Type
--------------------------------------------------------------------------------
import           Control.Lens      (use, (%=))
import           Control.Monad     (void, when)
import           Data.Int          (Int32)
import           Data.Map.Strict   as Map (insert, elems, lookup, null, fromList,
                                    alter)
import           Data.List         (intercalate)
import           Data.Monoid       ((<>))
import           Data.Text         (Text, unpack, pack)
import           Prelude           hiding (lookup)
import           Text.Megaparsec   (getPosition, lookAhead, notFollowedBy,
                                    sepBy, try, (<|>), optional, between)
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
type' = parenType <|> try userDefined <|> try arrayOf <|> try type''
  where
    parenType = do 
      t <- parens type'
      isPointer t
    -- Try to parse an array type
    arrayOf = do

      match TokArray
      msize <- arraySize
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
            Just (name, _, _) -> if name == id 
                then do
                  identifier
                  return $ GDataType name
                else do
                  notFollowedBy identifier
                  return GUndef
            Nothing -> do
              notFollowedBy identifier
              return GUndef

        Just ast@Struct {structName, structTypes, structProcs} -> do
          
          identifier
          fullTypes <- concat <$> (optional . parens $ type' `sepBy` match TokComma)

          let
            plen = length fullTypes
            slen = length structTypes
            show' []  = ""
            show'  l  = "(" <> intercalate "," (fmap show l) <> ")"
         
          ok <- if slen == plen 
            then pure True

            else do 
              if slen == 0
                then do 
                    putError from $ UnknownError $ "Type `" <> unpack structName <> 
                        "` does not expect " <> show' fullTypes <> " as argument"

              else if slen > plen
                then do 
                  putError from $ UnknownError $ "Type `" <> unpack structName <> 
                     "` expected " <> show slen <> " types " <> show' structTypes <>
                     "\n\tbut recived " <> show plen <> " " <> show' fullTypes

              else do
                  putError from $ UnknownError $ "Type `" <> unpack structName <> 
                       "` expected only " <> show slen <> " types as arguments " <> 
                       show' structTypes <> "\n\tbut recived " <> show plen <> " " <>
                       show' fullTypes

              pure False
          
          if ok
            then do
              let 
                types = Map.fromList $ zip structTypes fullTypes                  

                fAlter = \case 
                  Nothing -> Just [(types, ast)]
                  Just l ->  Just $ [(types, ast)] <> l
              fullDataTypes %= Map.alter fAlter structName

              pure $ GFullDataType structName types

            else pure GUndef

      

isPointer :: Type -> Parser Type
isPointer t = do
    match TokTimes
    isPointer (GPointer t)
  <|> pure t         

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
  <|> do {match TokSet;      match TokOf; GSet      <$> (typeVar<|>type') }
  <|> do {match TokMultiset; match TokOf; GMultiset <$> (typeVar<|>type') }
  <|> do {match TokSeq;      match TokOf; GSeq      <$> (typeVar<|>type') }

  <|> do {match TokFunc; ba <- (typeVar<|>type'); match TokArrow;   bb <- (typeVar<|>type'); pure $ GFunc ba bb }
  <|> do {match TokRel;  ba <- (typeVar<|>type'); match TokBiArrow; bb <- (typeVar<|>type'); pure $ GRel  ba bb }

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

