{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Program
  ( program
  ) where

-------------------------------------------------------------------------------
import           AST.Program
import           AST.Type
import           Common
import           Location           (Location (..))
import           Parser.Definition
import           Parser.Instruction (block)
import           Parser.Monad       hiding (sepBy1)
import           Parser.State
import           Parser.Struct
import           SymbolTable        (closeScope, openScope)
import           Token
-------------------------------------------------------------------------------
import           Control.Lens       (use, (%=))
import qualified Control.Monad      as M
import           Data.Either
import           Data.Foldable      (toList)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)
import           Data.Semigroup     ((<>))
import qualified Data.Sequence      as Seq (empty, fromList)
import qualified Data.Text          as T (intercalate)
import           Text.Megaparsec    (eof, getPosition, optional, sepBy1, (<|>))
-------------------------------------------------------------------------------

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'
program :: Parser (Maybe Program)
program = do
  from <- getPosition

  match' TokProgram
  name' <- safeIdentifier
  ext <- optional $ do
    match TokDot
    exts <- identifier `sepBy1` match TokDot
    pure $ "." <> T.intercalate "." exts
  match' TokBegin

  symbolTable %= openScope from -- Open the program scope
  many (abstractDataType <|> dataType)

  defs' <- sequence <$> many (function <|> procedure)
    -- (1) listDefProc should also include type definitions

  main' <- mainRoutine
  defs  <- Seq.fromList . toList <$> use definitions
  _moreDecls <- many (function <|> procedure)
    -- These aren't compiled since they can't be reached, but they're
    -- still checked so the user knows.

  match' TokEnd
  eof

  to <- getPosition
  symbolTable %= closeScope to

  case (name', main') of
    (Just name, Just main) -> do
      dts     <- use dataTypes
      fdts    <- use fullDataTypes
      strings <- use stringIds

      pure $ Just Program
        { name        = name <> fromMaybe "" ext
        , loc         = Location (from, to)
        , defs
        , insts       = main
        , structs     = dts
        , fullStructs = fdts
        , strings }

    _ -> pure Nothing

  where
    mainRoutine = match' TokMain *> block
