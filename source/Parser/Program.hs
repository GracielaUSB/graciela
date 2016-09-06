{-# LANGUAGE FlexibleContexts #-}

module Parser.Program
  ( program
  ) where

-------------------------------------------------------------------------------
import           AST.Program
import           Location           (Location (..))
import           Parser.Definition
import           Parser.Instruction (block)
import           Parser.Monad
import           Parser.State
import           SymbolTable        (closeScope, openScope)
import           Token
import           Type
import           Parser.ADT
-------------------------------------------------------------------------------
import qualified Control.Monad       as M
import qualified Data.Map            as Map
import           Control.Lens        ((%=),use)

import           Data.Either
import qualified Data.Sequence       as Seq (empty)
import qualified Data.Text           as T
import           Text.Megaparsec     ((<|>), eof, getPosition)
-------------------------------------------------------------------------------
import           Debug.Trace

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'
program :: Parser (Maybe Program)
program = do
  from <- getPosition
  
  match' TokProgram
  name' <- safeIdentifier
  match' TokBegin

  getPosition >>= \x -> symbolTable %= openScope x
  many (abstractDataType <|> dataType)
  
  decls' <- listDefProc
    -- (1) listDefProc should also include type definitions

  main' <- mainRoutine

  _moreDecls <- listDefProc
    -- These aren't compiled since they can't be reached, but they're
    -- still checked so the user knows.
  
  match' TokEnd

  eof
  to <- getPosition
  symbolTable %= closeScope to
  case (name', decls', main') of
    (Just name, Just decls, Just main) -> do

      dts  <- use dataTypes
      fdts <- use fullDataTypes
      pure . Just $ Program name (Location (from, to)) decls main dts fdts
    
    _ -> pure Nothing


  where
    mainRoutine = match' TokMain *> block
