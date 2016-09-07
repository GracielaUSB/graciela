{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Parser.Program
  ( program
  ) where

-------------------------------------------------------------------------------
import           AST.Program
import           Location           (Location (..))
import           Parser.ADT
import           Parser.Definition
import           Parser.Instruction (block)
import           Parser.Monad
import           Parser.State
import           SymbolTable        (closeScope, openScope)
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Lens       (use, (%=))
import qualified Control.Monad      as M
import qualified Data.Map.Strict    as Map

import           Data.Either
import qualified Data.Sequence      as Seq (empty)
import qualified Data.Text          as T
import           Text.Megaparsec    (eof, getPosition, (<|>))
-------------------------------------------------------------------------------
import           Debug.Trace

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'
program :: Parser (Maybe Program)
program = do
  from <- getPosition

  match' TokProgram
  name' <- safeIdentifier
  match' TokBegin

  symbolTable %= openScope from -- Open the program scope
  many (abstractDataType <|> dataType)

  defs' <- listDefProc
    -- (1) listDefProc should also include type definitions

  main' <- mainRoutine

  _moreDecls <- listDefProc
    -- These aren't compiled since they can't be reached, but they're
    -- still checked so the user knows.

  match' TokEnd
  eof

  to <- getPosition
  symbolTable %= closeScope to

  case (name', defs', main') of
    (Just name, Just defs, Just main) -> do
      dts     <- use dataTypes
      fdts    <- use fullDataTypes
      strings <- use stringIds
      pure $ Just Program
        { name
        , loc         = Location (from, to)
        , defs
        , insts       = main
        , structs     = dts
        , fullStructs = fdts
        , strings }

    _ -> pure Nothing

  where
    mainRoutine = match' TokMain *> block
