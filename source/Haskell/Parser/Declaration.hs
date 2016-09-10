{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Declarations
Description : Parseo y almacenamiento de las declaraciones
Copyright   : Graciela

Se encuentra todo lo referente al almacenamiento de las variables
en la tabla de simbolos, mientras se esta realizando el parser.
-}

module Parser.Declaration 
    ( declaration
    , polymorphicDeclaration
    , abstractDeclaration
    )
    where
-------------------------------------------------------------------------------
import           AST.Declaration           (Declaration (..))
import           AST.Expression            (Expression (..),
                                            Expression' (Value))
import           AST.Struct                (Struct(..))
import           Entry                     as E
import           Error
import           Location
import           Parser.Expression
import           Parser.Monad
-- import           Parser.Rhecovery
import           Parser.State
import           Parser.Type
import           SymbolTable
import           Token
import           AST.Type
--------------------------------------------------------------------------------
import           Control.Lens              (use, (%=))
import           Control.Monad             (foldM, forM_, unless, void, when,
                                            zipWithM_)
-- import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Trans.Class (lift)
import           Data.Functor              (($>))
import           Data.Monoid               ((<>))
import           Data.Map                  as Map (lookup)
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq (empty, fromList, null, zip)
import           Data.Text                 (Text, unpack)
import           Debug.Trace
import           Prelude                   hiding (lookup)
import           Text.Megaparsec           (getPosition, notFollowedBy,
                                            optional, try, (<|>), lookAhead)
--------------------------------------------------------------------------------
type Constness = Bool
-- | Se encarga del parseo de las variables y su almacenamiento en
-- la tabla de simbolos.

-- Only regular types
declaration :: Parser (Maybe Declaration)
declaration = declaration' type' False

-- Accept polymorphic types
polymorphicDeclaration :: Parser (Maybe Declaration)
polymorphicDeclaration = declaration' (try typeVar <|> type') True


-- Accept both, polymorphic and abstract types (set, function, ...)
abstractDeclaration :: Parser (Maybe Declaration)
abstractDeclaration = declaration' abstractType True

declaration' :: Parser Type -> Bool -> Parser (Maybe Declaration)
declaration' allowedTypes isStruct = do
  from <- getPosition

  isConst <- match TokConst $> True <|> match TokVar $> False
  ids <- identifierAndLoc `sepBy1` match TokComma
  mvals <- (if isConst then (Just <$>) else optional) assignment

  match TokColon
  t <- if isConst then type' else allowedTypes

  to <- getPosition
  let
    location = Location (from, to)

  if isConst && not (t =:= GOneOf [GBool, GChar, GInt, GFloat] )
    then do
      putError from . UnknownError $
        "Se intentó declarar constante de tipo `" <> show t <>
        "`, pero sólo se permiten constantes de tipos basicos."
      pure Nothing
    else case mvals of
      Nothing -> do
        -- Values were optional, and were not given
        forM_ ids $ \(id, loc) -> do
          redef <- redefinition (id, loc)
          unless redef  $ do
            struct <- use currentStruct
            let
              info = if isStruct
                then SelfVar t Nothing
                else Var t Nothing

              entry = Entry
                  { _entryName = id
                  , _loc       = loc
                  , _info      = info }
            symbolTable %= insertSymbol id entry
        pure . Just $ Declaration
          { declLoc  = location
          , declType = t
          , declIds  = fst <$> ids }

      Just Nothing -> do
        pure Nothing
        -- Values were either mandatory or optional, and were given, but
        -- had errors. No more errors are given.

      Just (Just exprs) ->
        -- Values were either mandatory or optional, but were given
        -- anyways, without errors in any.
        if length ids == length exprs
          then do
            pairs <- foldM (checkType isConst t) Seq.empty $ Seq.zip ids exprs
            pure $ if null pairs
              then Nothing
              else Just Initialization
                { declLoc   = location
                , declType  = t
                , declPairs = pairs }
          else do
            putError from . UnknownError $
              "La cantidad de " <>
              (if isConst then "constantes" else "variables") <>
              " es distinta a la de expresiones"
            pure Nothing


assignment :: Parser (Maybe (Seq Expression))
assignment = sequence <$>
  (match TokAssign *> expression `sepBy1` match TokComma)


checkType :: Constness -> Type
          -> Seq (Text, Expression)
          -> ((Text, Location), Expression)
          -> Parser (Seq (Text, Expression))
checkType True t pairs
  ((identifier, location), expr@Expression { expType, exp' }) = do
  

  let Location (from, _) = location
  redef <- redefinition (identifier,location)
      
  if expType =:= t
    then  if redef 
      then pure pairs
      else case exp' of
        Value v -> do
          let
            entry = Entry
              { _entryName  = identifier
              , _loc        = location
              , _info       = Const
                { _constType  = t
                , _constValue = v }}
          symbolTable %= insertSymbol identifier entry
          pure $ pairs |> (identifier, expr)
        _       -> do
          putError from . UnknownError $
            "Se intentó asignar una expresión no constante a la \
            \constante `" <> unpack identifier <> "`"
          pure Seq.empty

    else do
      putError from . UnknownError $
        "Se intentó asignar una expresión de tipo `" <> show expType <>
        "` a la constante `" <> unpack identifier <> "`, de tipo `" <>
        show t <> "`"
      pure Seq.empty

checkType False t pairs
  ((identifier, location), expr@Expression { loc, expType, exp' }) =

  let Location (from, _) = location
  in if expType =:= t
    then do
      redef <- redefinition (identifier,location)
      if redef
        then pure pairs
        else do
          let
            entry = Entry
              { _entryName  = identifier
              , _loc        = location
              , _info       = Var
                { _varType  = t
                , _varValue = Just expr }}
          symbolTable %= insertSymbol identifier entry
          pure $ pairs |> (identifier, expr)

    else do
      putError from . UnknownError $
        "Se intentó asignar una expresión de tipo `" <> show expType <>
        "` a la variable `" <> unpack identifier <> "`, de tipo `" <>
        show t <> "`"
      pure Seq.empty

redefinition :: (Text, Location) -> Parser Bool
redefinition (id, Location (from, _)) = do
  st <- use symbolTable
  let local = isLocal id st
  
  


  if local
    then do 
      putError from . UnknownError $
         "Redefinition of variable `" <> unpack id <> "`"
      pure True
    else do
      maybeStruct <- use currentStruct
      case maybeStruct of
        Just (_, Just abstName, _) -> do
          adt <- getStruct abstName
          case adt of
            Just abst -> do 
              if isLocal id . structSt $ abst
                then do 
                  putError from . UnknownError $
                    "Redefinition of variable `" <> unpack id <> 
                    "`. Was defined in Abstract Type `" <> unpack abstName <> "`"
                  pure True
                else pure False
            _ -> pure False
        _ -> pure False
          
