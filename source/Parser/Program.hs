{-# LANGUAGE FlexibleContexts #-}

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
import qualified Control.Monad       as M
import qualified Data.Map            as Map
import           Control.Lens        ((%=),use)
import qualified Data.Text           as T
import           Text.Megaparsec     ((<|>), eof, getPosition)
import Debug.Trace
-------------------------------------------------------------------------------

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'
program :: Parser (Maybe Program)
program = do
  from <- getPosition

  symbolTable %= openScope from
  many (abstractDataType <|> dataType)
  match TokProgram
  name' <- safeIdentifier
  match TokBegin

  decls' <- listDefProc
  body'  <- block
  match' TokEnd

  eof
  to <- getPosition
  symbolTable %= closeScope to

  case (name', decls', body') of
    (Just name, Just decls, Just body) -> do
      x  <- (Program name (Location(from, to)) decls body <$> use fullDataTypes)
      pure $ Just x 
    _ -> pure Nothing

