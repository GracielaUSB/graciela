{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Declarations
Description : Parseo y almacenamiento de las declaraciones
Copyright   : Graciela

Se encuentra todo lo referente al almacenamiento de las variables
en la tabla de simbolos, mientras se esta realizando el parser.
-}

module Language.Graciela.Parser.Declaration
    ( declaration
    , dataTypeDeclaration
    )
    where
-------------------------------------------------------------------------------
import           Language.Graciela.AST.Declaration   (Declaration (..))
import           Language.Graciela.AST.Expression    (Expression (..), Expression' (NullPtr, Value))
import           Language.Graciela.AST.Struct        (Struct (..), Struct' (..))
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.Entry
import           Language.Graciela.Error
import           Language.Graciela.Location
import           Language.Graciela.Parser.Expression
import           Language.Graciela.Parser.Monad
import           Language.Graciela.Parser.State
import           Language.Graciela.Parser.Type
import           Language.Graciela.SymbolTable
import           Language.Graciela.Token
--------------------------------------------------------------------------------
import           Control.Lens                        (over, use, (%=), (.~), _2,
                                                      _4, _Just)
import           Control.Monad                       (foldM, forM_, unless,
                                                      void, when, zipWithM_)
import           Control.Monad.Trans.Class           (lift)
import           Data.Map                            as Map (insert, lookup)
import           Data.Sequence                       (Seq, (|>))
import qualified Data.Sequence                       as Seq (empty, fromList,
                                                             null, zip)
import           Data.Text                           (Text, unpack)
import           Prelude                             hiding (lookup)
import           Text.Megaparsec                     (getPosition, lookAhead,
                                                      notFollowedBy, optional,
                                                      try, (<|>))
--------------------------------------------------------------------------------

type Constness = Bool
-- | Se encarga del parseo de las variables y su almacenamiento en
-- la tabla de simbolos.

-- Only regular types
declaration :: Parser (Maybe Declaration)
declaration = declaration' False

-- Accept polymorphic types
dataTypeDeclaration :: Parser (Maybe Declaration)
dataTypeDeclaration = declaration' True

declaration' :: Bool -> Parser (Maybe Declaration)
declaration' isStruct = do
  lookAhead $ oneOf [TokConst, TokLet, TokVar]

  isConst <- do
    f <- use useLet
    if f
      then match' TokLet $> False
      else match TokConst $> True <|> match TokVar $> False

  from <- getPosition

  ids <- identifierAndLoc `sepBy1` match TokComma
  mvals <- if isConst then assignment' else assignment
  forM_ ids $ \(name, Location (fromID,_)) -> do
    let good = foldl (\a b -> a && (b /= '\'')) True (unpack name)
    when (not good) . putError fromID . UnknownError $ 
      "Ilegal character `'` in variable name: " <> unpack name

  match TokColon
  t <- abstractType
  to <- getPosition
  isDeclarative' <- use isDeclarative

  maybeStruct <- use currentStruct
  let
    f' [x] = "the variable " <> f [x]
    f' xs  = "the variables " <> f xs
    f []              = ""
    f [(x,_)]         = "`" <> unpack x <> "`"
    f ((x,_):[(y,_)]) = "`" <> unpack x <> "` and `" <> unpack y <> "`"
    f ((x,_):xs)      = "`" <> unpack x <> "`, " <> f xs

  case maybeStruct of
    Just (dt@GDataType {typeName}, _, _, _) | recursiveDecl t dt ->
      putError from . UnknownError $
        "Attempting to declare " <> f' (toList ids) <>
        ",\n\twith a recursive type " <> show t <>
        ".\n\tPerhaps you want to declare a " <> (show $ GPointer t)
    _ -> pure ()

  let
    location = Location (from, to)
  if isConst && not (t =:= GOneOf [GBool, GChar, GInt, GFloat] )
    then do
      putError from . UnknownError $
        "Trying to declare a constant of type " <> show t <>
        ", but only basic types are allowed."
      pure Nothing
  else if not isDeclarative' && t =:= highLevel
    then do
      putError from . UnknownError $
        "Trying to declare a variable of type " <> show t <>
        " in imperative code"
      pure Nothing
    else case mvals of
      Nothing -> do
        -- Values were optional, and were not given
        forM_ ids $ \(name, loc) -> do
          redef <- redefinition (name, loc)
          unless redef  $ do
            info <- info' isStruct from name t Nothing False
            let
              entry = Entry
                { _entryName = name
                , _loc       = loc
                , _info      = info }
            symbolTable %= insertSymbol name entry

        pure . Just $ Declaration
          { declLoc  = location
          , declType = t
          , declIds  = fst <$> ids }

      Just Nothing  -> do
        pure Nothing
        -- Values were either mandatory or optional, and were given, but
        -- had errors. No more errors are given.

      Just (Just exprs) ->
        -- Values were either mandatory or optional, but were given
        -- anyways, without errors in any.
        if length ids == length exprs
          then do
            pairs' <- foldM (checkType isConst t isStruct) (Just Seq.empty) $ Seq.zip ids exprs
            pure $ case pairs' of
              Nothing -> Nothing
              Just pairs -> Just Initialization
                { declLoc   = location
                , declType  = t
                , declPairs = pairs }
          else do
            putError from . UnknownError $
              "The number of " <>
              (if isConst then "constants" else "variables") <>
              " do not match with the\n\tnumber of expressions to be assigned"
            pure Nothing

recursiveDecl :: Type -> Type -> Bool
recursiveDecl (GArray _ inner) dt = recursiveDecl inner dt
recursiveDecl t dt                = t =:= dt

assignment :: Parser (Maybe (Maybe (Seq Expression)))
assignment = optional $ sequence <$>
  (match TokAssign *> (expression `sepBy` match TokComma))

assignment' :: Parser (Maybe (Maybe (Seq Expression)))
assignment' = Just . sequence <$>
  (match' TokAssign *> (expression `sepBy` match TokComma))


checkType :: Constness -> Type -> Bool
          -> Maybe (Seq (Text, (Expression,Bool)))
          -> ((Text, Location), Expression)
          -> Parser (Maybe (Seq (Text, (Expression,Bool))))
checkType True t isStruct pairs
  ((name, location), expr@Expression { expType, expConst, exp' }) = do

  let Location (from, _) = location
  redef <- redefinition (name, location)

  if expType =:= t
    then if expConst
      then do
        info <- info' isStruct from name t (Just expr) True
        let
          entry = Entry
            { _entryName  = name
            , _loc        = location
            , _info       = info }
        unless redef $ symbolTable %= insertSymbol name entry
        pure $ (|> (name, (expr, True))) <$> pairs
    else do
      putError from . UnknownError $
        "Trying to assign a non constant expression to the \
        \constant `" <> unpack name <> "`."
      pure Nothing

  else do
    putError from . UnknownError $
      "Trying to assign an expression with type " <> show expType <>
      " to the constant `" <> unpack name <> "`, of type " <>
      show t <> "."
    pure Nothing

checkType False t isStruct pairs
  ((name, location), expr@Expression { loc, expType, exp' }) =

  let Location (from, _) = location
  in if expType =:= t
    then do
      redef <- redefinition (name,location)

      let
        expr' = case exp' of
          NullPtr {} -> expr{expType = t}
          _          -> expr
      unless redef $ do
        info <- info' isStruct from name t (Just expr') False
        let
          entry = Entry
            { _entryName  = name
            , _loc        = location
            , _info       = info }

        symbolTable %= insertSymbol name entry
      pure $ (|> (name, (expr',False))) <$> pairs

    else do
      putError from . UnknownError $
        "Trying to assign an expression with type " <> show expType <>
        " to the variable `" <> unpack name <> "`, of type " <>
        show t <> "."
      pure Nothing -- Seq.empty

redefinition :: (Text, Location) -> Parser Bool
redefinition (varName, Location (from, _)) = do
  st <- use symbolTable
  let local = isLocal varName st

  if local
    then do
      putError from . UnknownError $
         "Redefinition of variable `" <> unpack varName <> "`"
      pure True
    else do
      maybeStruct <- use currentStruct
      case maybeStruct of
        Just (GDataType _ (Just abstName) _, _, _, _) -> do
          adt <- getStruct abstName
          case adt of
            Just abst -> do
              if isLocal varName . structSt $ abst
                then do
                  -- putError from . UnknownError $
                  --   "Redefinition of variable `" <> unpack varName <>
                  --   "`. Was defined in Abstract Type `" <> unpack abstName <> "`"
                  pure False --
                else pure False
            _ -> pure False
        _ -> pure False



info' :: Bool -> SourcePos -> Text
      -> Type -> Maybe Expression -> Bool
      -> Parser Entry'
info' isStruct pos name t expr constness = if isStruct
  then do
    Just (_ , fields, _, dFields) <- use currentStruct
    let
      f l = (fromIntegral l, t, constness, expr)
      fields'    = Map.insert name (f $ length fields) fields
      dFields' l = Map.insert name (f l) dFields
    case name `Map.lookup` fields of
      Just (p, t', c, _)
        | c /= constness ->
          let
            aux a = if a then "constant" else "variable"
          in
            putError pos . UnknownError $
            "Redefinition of member `" <> unpack name <> "` as " <> aux constness <>
            ",\n\tbut defined in abstract type as " <> aux c <> "."

        | t =:= highLevel -> do
          putError pos . UnknownError $
            "Redefinition of member `" <> unpack name <> "` already defined in abstract type."


        | t' =:= t -> currentStruct %= over _Just (_4 .~ dFields' p)

      Just _ -> putError pos . UnknownError $
        "Ambigous redefinition of variable `" <> unpack name <> "` defined in abstract type"
      _ -> currentStruct %= over _Just (_2 .~ fields')
    pure $ SelfVar t expr constness
  else pure $ Var t expr constness
