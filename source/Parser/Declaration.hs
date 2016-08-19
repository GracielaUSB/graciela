{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Declarations
Description : Parseo y almacenamiento de las declaraciones
Copyright   : Graciela

Se encuentra todo lo referente al almacenamiento de las variables
en la tabla de simbolos, mientras se esta realizando el parser.
-}
module Parser.Declaration (declaration) where

-------------------------------------------------------------------------------
import           AST.Declaration                (Declaration (..))
import           AST.Expression                 (Expression (..),
                                                 Expression' (Value))
import           Entry                          as E
import           Error
import           Graciela
import           Location
import           Parser.Expression
import           Parser.Recovery
import           Parser.Token
import           Parser.Type
import           SymbolTable
import           Token
import           Type
--------------------------------------------------------------------------------
import           Control.Lens                   (use, (%=))
import           Control.Monad                  (foldM, forM_, unless, void,
                                                 when, zipWithM_)
import           Control.Monad.Trans.State.Lazy
-- import           Data.Functor.Identity
import           Data.Functor                   (($>))
import           Data.Monoid                    ((<>))
import           Data.Sequence                  (Seq, (|>))
import qualified Data.Sequence                  as Seq (empty, fromList, null)
import           Data.Text                      (Text, unpack)
import           Prelude                        hiding (lookup)
import           Text.Megaparsec                (getPosition, notFollowedBy,
                                                 optional, sepBy, sepBy1, try,
                                                 (<|>))
import           Debug.Trace
--------------------------------------------------------------------------------
type Constness = Bool
-- | Se encarga del parseo de las variables y su almacenamiento en
-- la tabla de simbolos.
declaration :: Graciela Declaration
declaration = do
  from <- getPosition
  isConst <- match TokConst $> True <|> match TokVar $> False
  ids <- identifierAndLoc `sepBy1` match TokComma

  mvals <- (if isConst then (Just <$>) else optional) assignment

  match TokColon
  t <- if isConst then basicType else type'
  to <- getPosition

  let
    location = Location (from, to)

  if isConst && t == GUndef
    then do
      putError location $ UnknownError $
        "Se intentó declarar constante de tipo `" <> show t <>
        "`, pero sólo se permiten constantes de tipos basicos."
      pure $ BadDeclaration location
    else case mvals of
      Nothing -> do
        forM_ ids $ \(id, loc) -> do
          redefinition (id, loc)
          let 
            entry = Entry
              { _entryName = id
              , _loc       = loc
              , _info      = Var t Nothing }          
          symbolTable %= insertSymbol id entry
        pure Declaration
          { declLoc  = location
          , declType = t
          , declIds  = fmap fst . Seq.fromList $ ids }

      Just exprs ->
        if length ids == length exprs
          then do
            pairs <- foldM (checkType isConst t) Seq.empty $ zip ids exprs
            pure $ if Seq.null pairs
              then BadDeclaration location
              else Initialization
                { declLoc   = location
                , declType  = t
                , declPairs = pairs }
          else do
            putError location $ UnknownError $
              "La cantidad de " <>
              (if isConst then "constantes" else "variables") <>
              " es distinta a la de expresiones"
            pure $ BadDeclaration location

assignment :: Graciela [Expression]
assignment = match TokAssign *> expression `sepBy1` match TokComma

checkType :: Constness -> Type
          -> Seq (Text, Expression) 
          -> ((Text, Location), Expression)
          -> Graciela (Seq (Text, Expression))
checkType _ _ _ (_, BadExpression {}) = pure Seq.empty

checkType True t pairs
  ((identifier, location), expr@Expression { expType, exp' }) = do
  redefinition (identifier,location)
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
        putError location $ UnknownError $
          "Se intentó asignar una expresión no constante a la \
          \constante `" <> unpack identifier <> "`"
        pure Seq.empty
    else do
      putError location $ UnknownError $
        "Se intentó asignar una expresión de tipo `" <> show expType <>
        "` a la constante `" <> unpack identifier <> "`, de tipo `" <>
        show t <> "`"
      pure Seq.empty

checkType False t pairs
  ((identifier, location), expr@Expression { loc, expType, exp' }) =
  if expType =:= t
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
      putError location $ UnknownError $
        "Se intentó asignar una expresión de tipo `" <> show expType <>
        "` a la variable `" <> unpack identifier <> "`, de tipo `" <>
        show t <> "`"
      pure Seq.empty

redefinition :: (Text, Location) -> Graciela ()
redefinition (id, location) = do
  st <- use symbolTable
  let local = isLocal id st
  traceM $ show local <> unpack id
  when local $ 
    putError location $ UnknownError $ 
      "Redefinition of variable `" <> unpack id <> "`"
