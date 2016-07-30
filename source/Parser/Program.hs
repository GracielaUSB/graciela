module Parser.Program
  ( program
  ) where


-------------------------------------------------------------------------------
import           AST
import           Graciela
import qualified MyParseError        as PE
import           Parser.ADT
import           Parser.Instruction (block)
import           Parser.Procedure   
import           Parser.Token
import           ParserState
import           Token
import           Type
-------------------------------------------------------------------------------
import qualified Control.Monad       as M
import qualified Data.Text           as T
import           Text.Megaparsec     hiding (Token)
-------------------------------------------------------------------------------

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'
mainProgram :: Graciela AST
mainProgram = do
  posFrom <- getPosition
  match TokProgram
  id <- identifier
  match TokBegin

  decls <- listDefProc (match TokOpenBlock)
  body  <- block (match TokEnd)
  try ( do match TokEnd
           eof
           posTo <- getPosition
           return (AST posFrom posTo GEmpty (Program id decls body))
      )
      <|> do --genNewError eof PE.LexEnd
             return (AST posFrom posFrom GError (EmptyAST))


-- Program -> Abstract Program
-- Program -> MainProgram
{- The program consists in a set of Abstract Data Types, Data Types and a main program -}
program :: Graciela (Maybe AST)
program = do
  newScopeParser
  many (abstractDataType <|> dataType) -- Por ahora debe haber un programa
  ast <- mainProgram                   -- principal al final del archivo
  return $ Just $ ast
