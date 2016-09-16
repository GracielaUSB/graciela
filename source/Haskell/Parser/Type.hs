{-# LANGUAGE LambdaCase     #-}
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
import           AST.Definition    (Definition (..), Definition' (..))
import           AST.Expression    (Expression (..), Expression' (Value),
                                    Value (..))
import           AST.Struct
import           AST.Type
import           Entry
import           Error
import           Location
import           Parser.Expression (expression)
import           Parser.Monad      (Parser, getStruct, getType, identifier,
                                    integerLit, match, match', parens, putError)
import           Parser.State
import           SymbolTable       (lookup)
import           Token
--------------------------------------------------------------------------------
import           Control.Lens      (use, (%=), (<>=))
import           Control.Monad     (void, when)
import qualified Data.Array        as Array (listArray)
import           Data.Int          (Int32)
import           Data.List         (elemIndex, intercalate)
import qualified Data.Map.Strict   as Map (alter, elems, fromList, insert,
                                           lookup, null, singleton)
import           Data.Monoid       ((<>))
import           Data.Text         (Text, pack, unpack)
import           Prelude           hiding (lookup)
import           Text.Megaparsec   (between, getPosition, lookAhead,
                                    notFollowedBy, optional, sepBy, try, (<|>))
--------------------------------------------------------------------------------
import           Debug.Trace

basicType :: Parser Type
basicType = do
  from <- getPosition
  t <- type'
  if t =:= GOneOf [GBool, GChar, GInt, GFloat]
    then pure t
    else do
      putError from . UnknownError $ show t <> " is not a basic type"
      pure GUndef


type' :: Parser Type
type' = parenType <|> try typeVar <|> try userDefined <|> try arrayOf <|> try type''
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
            Just (GDataType name abstract _, _, _) -> do
              if name == id
                then do
                  identifier
                  t <- parens $ type' `sepBy` match TokComma
                  let typeargs = Array.listArray (0, length t - 1) t
                  isPointer $ GDataType name abstract typeargs
                else do
                  notFollowedBy identifier
                  return GUndef
            Nothing -> do
              notFollowedBy identifier
              return GUndef

        Just ast@Struct {structBaseName, structTypes, structProcs} -> do

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
                then
                    putError from . UnknownError $ "Type `" <> unpack structBaseName <>
                        "` does not expect " <> show' fullTypes <> " as argument"

              else if slen > plen
                then
                  putError from . UnknownError $ "Type `" <> unpack structBaseName <>
                     "` expected " <> show slen <> " types " <> show' structTypes <>
                     "\n\tbut recived " <> show plen <> " " <> show' fullTypes

              else
                  putError from . UnknownError $ "Type `" <> unpack structBaseName <>
                       "` expected only " <> show slen <> " types as arguments " <>
                       show' structTypes <> "\n\tbut recived " <> show plen <> " " <>
                       show' fullTypes

              pure False

          if ok
            then do
              let
                types = Array.listArray (0, plen - 1) fullTypes
              
              when (null $ filter isTypeVar fullTypes) $ do
                let
                  fAlter = \case
                    Nothing               -> Just (ast, [types])
                    Just (struct, types0) -> Just (struct, types : types0 )

                fullDataTypes %= Map.alter fAlter structBaseName

              isPointer $ GFullDataType structBaseName types

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
   =  do {match TokSet;      match TokOf; GSet      <$> (typeVar<|>type') }
  <|> do {match TokMultiset; match TokOf; GMultiset <$> (typeVar<|>type') }
  <|> do {match TokSeq;      match TokOf; GSeq      <$> (typeVar<|>type') }

  <|> do {match TokFunc; ba <- typeVar<|>type'; match TokArrow;   bb <- typeVar<|>type'; pure $ GFunc ba bb }
  <|> do {match TokRel;  ba <- typeVar<|>type'; match TokBiArrow; bb <- typeVar<|>type'; pure $ GRel  ba bb }

  <|> (GTuple <$> parens (typeVar `sepBy` match TokComma))

  <|> type'


typeVarDeclaration  :: Parser Type
typeVarDeclaration = do
  tname <- lookAhead identifier
  pos   <- getPosition
  t     <- getType tname
  case t of
    Nothing -> do
      identifier
      notFollowedBy (match TokTimes)

      use typeVars >>= \x -> if tname `elem` x
        then do
          putError pos . UnknownError $
            "Reused type variable `" <> unpack tname <> "`."
          pure GUndef
        else do
          tvs <- use typeVars
          typeVars <>= [tname]
          pure $ GTypeVar (length tvs) tname
    Just _ -> do
      notFollowedBy identifier
      pure GUndef


typeVar :: Parser Type
typeVar = do
  tname <- lookAhead identifier
  tvars <- use typeVars
  case tname `elemIndex` tvars of
    Nothing -> do
      notFollowedBy identifier
      pure GUndef
    Just i -> do
      identifier
      isPointer $ GTypeVar i tname
