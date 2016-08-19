{-# LANGUAGE FlexibleContexts#-}

module Parser.Program
  ( program
  ) where


-------------------------------------------------------------------------------
import           AST.Program
import           Graciela
import           Parser.ADT
import           Parser.Instruction (block)
import           Parser.Recovery
import           Parser.Definition   
import           Parser.Token
import           SymbolTable         (openScope, closeScope)
import           Location            (Location(..))
import           Token
import           Type
-------------------------------------------------------------------------------
import qualified Control.Monad       as M
import           Control.Lens        ((%=))
import qualified Data.Text           as T
import           Text.Megaparsec     ((<|>),many, eof, getPosition)
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
  
