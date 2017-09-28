{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Language.Graciela.Parser.Type
    ( basicType
    , basicOrPointer
    , type'
    , abstractType
    , typeVarDeclaration
    , typeVar
    ) where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Declaration
import           Language.Graciela.AST.Definition    (Definition (..),
                                                      Definition' (..))
import           Language.Graciela.AST.Expression    (Expression (..),
                                                      Expression' (Value),
                                                      Value (..))
import           Language.Graciela.AST.Struct
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.Entry
import           Language.Graciela.Error
import           Language.Graciela.Location
import           Language.Graciela.Parser.Expression (expression)
import           Language.Graciela.Parser.Monad      (Parser, followedBy,
                                                      getStruct, getType,
                                                      identifier,
                                                      identifierAndLoc,
                                                      integerLit, match, match',
                                                      oneOf, parens, putError,
                                                      sepBy)
import           Language.Graciela.Parser.State
import           Language.Graciela.SymbolTable       (insertSymbol, local,
                                                      lookup)
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Control.Lens                        (use, (%=), (<>=))
import qualified Data.Array                          as Array (listArray)
import           Data.Foldable                       (asum, toList)
import           Data.Int                            (Int32)
import           Data.List                           (elemIndex, intercalate)
import qualified Data.Map.Strict                     as Map
import           Data.Set                            as Set (fromList, insert,
                                                            member)

import           Data.Text                           (Text, pack, unpack)
import           Prelude                             hiding (lookup)
import           Text.Megaparsec                     (between, getPosition,
                                                      lookAhead, notFollowedBy,
                                                      optional, try, (<|>))
--------------------------------------------------------------------------------

basicType :: Parser Type
basicType = do
  from <- getPosition
  t <- type'
  if t =:= GOneOf [GBool, GChar, GInt, GFloat, GATypeVar]
    then pure t
    else do
      putError from . UnknownError $ show t <> " is not a basic type"
      pure GUndef

basicOrPointer :: Parser Type
basicOrPointer = do
  from <- getPosition
  t <- type'
  if t =:= GOneOf [GBool, GChar, GInt, GFloat, GATypeVar, GPointer GAny]
    then pure t
    else do
      putError from . UnknownError $ show t <> " is not a basic or pointer type"
      pure GUndef


type' :: Parser Type
type' =  try parenType
     <|> try arrayOf
     <|> try enumerationOrAlias
     <|> try userDefined
     <|> try typeVar
     <|> try basicOrPtr
  where
    enumerationOrAlias = do
      from  <- getPosition
      tname <- lookAhead identifier
      to    <- getPosition
      use userDefinedType >>= \t -> case Map.lookup tname t of
        Just t -> do 
          void $ identifier
          isPointer $ t
        Nothing -> do
          match TokLeftPar -- This always fails
          pure GUndef
        t -> internal $ show t

    parenType = do
      lookAhead $ match TokLeftPar
      t <- parens type'
      isPointer t

    -- Try to parse an array type
    arrayOf :: Parser Type
    arrayOf = do
      lookAhead $ match TokArray
      pos <- getPosition
      match TokArray

      mdims' <- between (match' TokLeftBracket) (match' TokRightBracket) $
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

        t | t =:= basic || t =:= GPointer GAny || t =:= GATypeVar -> case mdims of
          Nothing -> pure GUndef
          Just dims -> if null dims
            then do
              putError pos' . UnknownError $
                "Missing dimensions in array declaration."
              pure GUndef
            else pure $ GArray dims t

        t -> do
          putError pos . UnknownError $
            "Arrays of " <> show t <> " are not supported"
          pure GUndef

    arraySize :: Parser (Maybe Expression)
    arraySize = do
      pos <- getPosition
      expr <- expression `followedBy` oneOf [TokComma, TokRightBracket]
      case expr of
        Nothing ->
          pure Nothing
        Just e@Expression { expType, loc, expConst, exp' } -> case expType of
          GInt {-| expConst-} -> case exp' of
            Value (IntV i) | i <= 0 -> do
              putError pos . UnknownError $
                "A non-positive dimension was given in the array declaration."
              pure Nothing
            _ -> pure . Just $ e
          -- t -> do
          --   putError pos . UnknownError $
          --     "Array dimension must be an integer constant expression."
          --   pure Nothing
    basicOrPtr = do
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
      name <- lookAhead identifier
      getStruct name >>= \case
        Nothing -> do
          current <- use currentStruct
          case current of
            Just (dt@(GDataType dtName abstract typeargs'), _, _, _, _) -> do
              if dtName == name
                then do
                  identifier
                  t <- (optional . parens $
                    (try typeVar <|> basicOrPointer) `sepBy` match TokComma)
                        >>= \case
                          Just s -> pure $ toList s
                          _      -> pure []

                  let
                    typeargs = Array.listArray (0, length t - 1) t

                  unless (null t || any (=:= GATypeVar) t) $ do
                    let
                      t = Map.fromList [(typeargs, False)]
                      fAlter = Just . \case
                        Nothing     -> t
                        Just types0 -> types0 `Map.union` t

                    fullDataTypes %= Map.alter fAlter dtName
                 
                  checkTypeArgs from dtName typeargs typeargs'

                  isPointer $ GDataType name abstract typeargs

                else do
                  notFollowedBy identifier
                  return GUndef
            Nothing -> do
              notFollowedBy identifier
              return GUndef

        Just ast@Struct {structBaseName, structTypes, structProcs, struct'} -> do
          identifier
          fullTypes <- asum <$> (optional . parens $ basicOrPointer `sepBy` match TokComma)

          ok <- checkTypeArgs from structBaseName fullTypes structTypes
        
          let
            abstName = case struct' of
              AbstractDataType{} -> Nothing
              _                  -> Just $ abstract struct'

          if ok
            then do
              let
                types = Array.listArray (0, (length fullTypes) - 1) . toList $ fullTypes
                dataType = GDataType structBaseName abstName types

              when (isNothing abstName) . putError from . UnknownError $
                "Trying to define a variable of abstract type " <>
                show dataType <>
                ".\n\tAbstract data types can only be used inside its own definition."

              if (any (=:= GATypeVar) fullTypes)
                then do
                  current <- use currentStruct
                  case current of
                    Just ((GDataType dtName _ _), _, _, _, _) -> do
                      let
                        fAlter = Just . \case
                          Nothing    -> Set.fromList [structBaseName]
                          Just names -> Set.insert structBaseName names
                      pendingDataType %= Map.alter fAlter dtName
                    Nothing -> pure ()

                else do
                  let
                    t = Map.fromList [(types, False)]
                    fAlter = Just . \case
                      Nothing     -> t
                      Just types0 -> types0 `Map.union` t

                  fullDataTypes %= Map.alter fAlter structBaseName

              isPointer dataType

            else pure GUndef
      where 
        checkTypeArgs :: (Foldable m, Functor m, Foldable n, Functor n) 
          => SourcePos -> Text 
          -> m Type -> n Type 
          -> Parser Bool
        checkTypeArgs from dtName typeargs typeargs' 
          | length typeargs == length typeargs' = pure True
          | length typeargs /= length typeargs' = do 
            let 
              plen = length typeargs
              slen = length typeargs'
              show' l = if null l
                then ""
                else "(" <> intercalate "," (toList $ show <$> l) <> ")"
            if slen == 0 then putError from . UnknownError $
              "Type `" <> unpack dtName <>
              "` does not expect " <> show' typeargs <> " as argument"

            else if slen > plen then putError from . UnknownError $
              "Type `" <> unpack dtName <> "` expected " <>
              show slen <> " types " <> show' typeargs' <>
              "\n\tbut recived " <> if plen == 0 then "none." else (show plen <> " " <> show' typeargs)

            else putError from . UnknownError $
              "Type `" <> unpack dtName <> "` expected only " <>
              show slen <> " types as arguments " <> show' typeargs' <>
              "\n\tbut recived " <> show plen <> " " <> show' typeargs
            pure False




isPointer :: Type -> Parser Type
isPointer t = do
    match TokTimes
    isPointer (GPointer t)
  <|> pure t


abstractType :: Parser Type
abstractType = do
  t <- try (parens abstractType) <|> abstractType'
  isPointer t
  where
    abstractType'
       =  do {match TokSet;      match' TokOf; GSet      <$> allowedType }
      <|> do {match TokMultiset; match' TokOf; GMultiset <$> allowedType }
      <|> do {match TokSequence; match' TokOf; GSeq      <$> allowedType }

      <|> do {match TokFunction; ba <- allowedType; match' TokArrow;   bb <- allowedType; pure $ GFunc ba bb }
      <|> do {match TokRelation; ba <- allowedType; match' TokBiArrow; bb <- allowedType; pure $ GRel  ba bb }

      <|> do {match TokLeftPar; a <- allowedType; match TokComma; b <- allowedType; match TokRightPar; pure $ GTuple a b}
      <|> do
        pos <- getPosition
        match TokLeftPar
        t1 <- type'
        lookAhead $ match TokComma
        match TokComma
        t2 <- type'
        match TokRightPar
        when (not (t1 =:= basic && t1 =:= basic)) . putError pos . UnknownError $
                  "Only tuples of basic types are allowed.\n" <> show (t1,t2) <> "was given"
        pure $ GTuple t1 t2
      <|> type'

    allowedType = do
      from <- getPosition
      t <- type'
      unless (t =:= GOneOf [GATypeVar, GBool, GChar, GInt, GFloat, GPointer GAny]) .
        putError from . UnknownError $ "Unexpected type " <> show t <>
        ".\n\tCollections can contain elements of basic type or pointers"
      pure t

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
  pos   <- getPosition
  tname <- lookAhead identifier
  tvars <- use typeVars
  existsDT' <- use existsDT
  case tname `elemIndex` tvars of
    Nothing -> do
      notFollowedBy identifier
      pure GUndef
    Just i
      | existsDT' -> do
        identifier
        isPointer $ GTypeVar i tname

      | otherwise -> do
        Just (dt,_,_,_,_) <- use currentStruct
        identifier
        putError pos . UnknownError $
          "To use a variable of type " <> show (GTypeVar i tname) <>
          "\n\tone of the method's parameter must be of type " <> show dt
        pure $ GTypeVar i tname
