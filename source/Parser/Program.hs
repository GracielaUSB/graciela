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
import           Parser.Recovery
import           SymbolTable        (closeScope, openScope)
import           Token
import           Type
-------------------------------------------------------------------------------
import           Control.Lens       ((%=))
import qualified Control.Monad      as M
import qualified Data.Text          as T
import           Text.Megaparsec    (eof, getPosition, many, (<|>))
-------------------------------------------------------------------------------

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'
program :: Graciela Program
program = do
  from <- getPosition
  symbolTable %= openScope from
  structs <- many (abstractDataType <|> dataType)

  withRecovery TokProgram
  id <- safeIdentifier
  TokBegin `withRecoveryFollowedBy` oneOf [TokProc, TokFunc, TokOpenBlock]

  decls <- listDefProc
  body  <- block
  withRecovery TokEnd
  eof
  to <- getPosition
  symbolTable %= closeScope to
  return $ Program id (Location(from, to)) decls body structs
