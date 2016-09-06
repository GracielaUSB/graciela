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
  ) where
-------------------------------------------------------------------------------
import           AST.Declaration           (Declaration (..))
import           AST.Expression            (Expression (..),
                                            Expression' (Value))
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
import           Type
--------------------------------------------------------------------------------
import           Control.Lens              (use, (%=))
import           Control.Monad             (foldM, forM_, unless, void, when,
                                            zipWithM_)
-- import           Control.Monad.Trans.State.Lazy
import           Control.Monad.Trans.Class (lift)
import           Data.Functor              (($>))
import           Data.Monoid               ((<>))
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq (empty, fromList, null, zip)
import           Data.Text                 (Text, unpack)
import           Debug.Trace
import           Prelude                   hiding (lookup)
import           Text.Megaparsec           (getPosition, notFollowedBy,
                                            optional, try, (<|>))
--------------------------------------------------------------------------------
type Constness = Bool
-- | Se encarga del parseo de las variables y su almacenamiento en
-- la tabla de simbolos.

declaration :: Parser (Maybe Declaration)
declaration = do
  from <- getPosition
  isConst <- match TokConst $> True <|> match TokVar $> False
  ids <- identifierAndLoc `sepBy1` match TokComma

  mvals <- (if isConst then (Just <$>) else optional) assignment

  match TokColon
  -- t' <- if isConst then basicType else type'
  t <- type'
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
          redefinition (id, loc)
          let
            entry = Entry
              { _entryName = id
              , _loc       = loc
              , _info      = Var t Nothing }
          symbolTable %= insertSymbol id entry
        pure . Just $ Declaration
          { declLoc  = location
          , declType = t
          , declIds  = fst <$> ids }

      Just Nothing -> pure Nothing
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
  redefinition (identifier,location)

  let Location (from, _) = location

  if expType =:= t
    then case exp' of
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
      redefinition (identifier,location)
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

redefinition :: (Text, Location) -> Parser ()
redefinition (id, Location (from, _)) = do
  st <- use symbolTable
  let local = isLocal id st
  when local .
    putError from . UnknownError $
      "Redefinition of variable `" <> unpack id <> "`"
