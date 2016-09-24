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
import           AST.Expression    (Expression' (..), Expression'' (Value),
                                    Value (..))
import           AST.Struct
import           AST.Type
import           Entry
import           Error
import           Location
import           Parser.Expression (expression)
import           Parser.Monad      (Parser, followedBy, getStruct, getType,
                                    identifier, identifierAndLoc, integerLit,
                                    match, match', oneOf, parens, putError,
                                    sepBy)
import           Parser.State
import           SymbolTable       (insertSymbol, local, lookup)
import           Token
--------------------------------------------------------------------------------
import           Control.Lens      (use, (%=), (<>=))
import           Control.Monad     (unless)
import qualified Data.Array        as Array (listArray)
import           Data.Foldable     (asum, toList)
import           Data.Int          (Int32)
import           Data.List         (elemIndex, intercalate)
import qualified Data.Map.Strict   as Map (alter, elems, fromList, insert,
                                           lookup, null, singleton)
import           Data.Semigroup ((<>))
import           Data.Text         (Text, pack, unpack)
import           Prelude           hiding (lookup)
import           Text.Megaparsec   (between, getPosition, lookAhead,
                                    notFollowedBy, optional, try, (<|>))
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
type' =  parenType
     <|> try typeVar
     <|> try userDefined
     <|> try arrayOf
     <|> try basicOrPointer
  where
    parenType = do
      t <- parens type'
      isPointer t

    -- Try to parse an array type
    arrayOf :: Parser Type
    arrayOf = do
      pos <- getPosition
      match TokArray
      mdims' <- between (match TokLeftBracket) (match' TokRightBracket) $
        arraySize `sepBy` match TokComma
      match TokOf
      t <- type'
      pos' <- getPosition

      let mdims = sequence mdims'

      case t of
        GUndef -> pure GUndef
        GArray _ _ -> do
          putError pos . UnknownError $
            "Cannot build an array of arrays. \
            \Try instead a multidimensional array."
          pure GUndef
        t -> case mdims of
          Nothing -> pure GUndef
          Just dims -> if null dims
            then do
              putError pos' . UnknownError $
                "Missing dimensions in array declaration."
              pure GUndef
            else pure $ GArray dims t

    arraySize :: Parser (Maybe Expression)
    arraySize = do
      pos <- getPosition
      expr <- expression
      case expr of
        Nothing ->
          pure Nothing
        Just e@Expression { expType, loc, expConst, exp' } -> case expType of
          GInt | expConst -> case exp' of
            Value (IntV i) | i <= 0 -> do
              putError pos . UnknownError $
                "A negative dimension was given in the array declaration."
              pure Nothing
            _ -> pure . Just $ e
          _ -> do
            putError pos . UnknownError $
              "Array dimension must be an integer constant expression."
            pure Nothing

    sizeExpr = do
      pos <- getPosition
      expr <- expression `followedBy` oneOf [TokComma, TokRightBracket]
      case expr of
        Nothing ->
          pure Nothing
        Just e@Expression { exp' } -> case exp' of
          Value (IntV i) -> if i <= 0
            then do
              putError pos . UnknownError $
                "A negative dimension was given in the array declaration."
              pure Nothing
            else pure . Just . Right $ e
          _ -> do
            putError pos . UnknownError $
              "Array dimension must be an integer constant expression."
            pure Nothing

    basicOrPointer = do
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
                  let typeargs = Array.listArray (0, length t - 1) (toList t)
                  isPointer $ GDataType name abstract typeargs
                else do
                  notFollowedBy identifier
                  return GUndef
            Nothing -> do
              notFollowedBy identifier
              return GUndef

        Just ast@Struct {structBaseName, structTypes, structProcs} -> do

          identifier
          fullTypes <- asum <$> (optional . parens $ type' `sepBy` match TokComma)

          let
            plen = length fullTypes
            slen = length structTypes
            show' l = if null l
              then ""
              else "(" <> intercalate "," (toList $ show <$> l) <> ")"

          ok <- if slen == plen
            then pure True

            else do
              if slen == 0 then putError from . UnknownError $
                "Type `" <> unpack structBaseName <>
                "` does not expect " <> show' fullTypes <> " as argument"

              else if slen > plen then putError from . UnknownError $
                "Type `" <> unpack structBaseName <> "` expected " <>
                show slen <> " types " <> show' structTypes <>
                "\n\tbut recived " <> show plen <> " " <> show' fullTypes

              else putError from . UnknownError $
                "Type `" <> unpack structBaseName <> "` expected only " <>
                show slen <> " types as arguments " <> show' structTypes <>
                "\n\tbut recived " <> show plen <> " " <> show' fullTypes

              pure False

          if ok
            then do
              let
                types = Array.listArray (0, plen - 1) . toList $ fullTypes

              unless (any isTypeVar fullTypes) $ do
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
