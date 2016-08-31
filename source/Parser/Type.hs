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
import           Graciela
import           Location
import           Parser.Expression (expression)
import           Parser.Recovery
import           Parser.Token      (identifier, integerLit, match, parens)
import           SymbolTable       (lookup)
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Lens      (use, (%=))
import           Control.Monad     (void, when)
import           Data.Int          (Int32)
import           Data.Map          as Map (insert, elems, lookup)
import           Data.List         (intercalate)
import           Data.Monoid       ((<>))
import           Data.Text         (Text, unpack)
import           Prelude           hiding (lookup)
import           Text.Megaparsec   (getPosition, lookAhead, notFollowedBy,
                                    sepBy, try, (<|>), optional)
import         Debug.Trace
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
        
    -- TODO: Or if its a user defined Type
    userDefined = do
      from <- getPosition
      id <- lookAhead identifier
      t  <- getStruct id

      case t of
        Nothing -> do
          current <- use currentStruct 
          case current of 
            Just (name, t) -> if name == id 
                then do
                  ok <- checkType name t from
                  if ok
                    then do 
                      return $ GDataType name t
                    else return GUndef
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
                  mapM (\x -> definitions %= insert (defName x) x) structProcs
                  return $ GDataType structName structTypes
                Just x -> return $ GDataType structName structTypes
            else return GUndef
    
    checkType name types from = do
      identifier
      polymorphism <- do 
          list <- optional $ match TokOf >> 
                       (type' >>= return . (:[]) ) <|> 
                       (parens $ type' `sepBy` match TokComma)
          return . concat $ list

      to <- getPosition
      let
        loc  = Location (from,to)
        plen = length polymorphism
        slen = length types
        show' [a] = show a
        show'  l  = intercalate "," (fmap show l)
     
      if slen == plen 
        then return True

      else if slen == 0
        then do 
          putError loc $ UnknownError $ "Type `" <> unpack name <> 
              "` does not expect " <> show' polymorphism <> " as argument"
          return False

      else if slen > plen
        then do 
          putError loc $ UnknownError $ "Type `" <> unpack name <> 
             "` expected " <> show slen <> " types " <> show' types <>
             "\n\tbut recived only " <> show plen
          return False

      else do
          putError loc $ UnknownError $ "Type `" <> unpack name <> 
               "` expected only " <> show slen <> " types as arguments " <> 
               show' types <> "\n\tbut recived " <> show plen <> " " <>
               show' polymorphism
          return False
         
          




isPointer :: Type -> Graciela Type
isPointer t = do
    match TokTimes
    isPointer (GPointer t)
  <|> pure t

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


typeVarDeclaration  :: Graciela Type
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

typeVar :: Graciela Type
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
