module Parser.Program
  ( program
  ) where


-------------------------------------------------------------------------------
import           AST
import           Graciela
import           MyParseError        as PE
import           Parser.ADT
import           Parser.Instructions (block)
import           Parser.Procedures   (listDefProc, panicMode, panicModeId)
import           Parser.Token
import           ParserState
import           Token
import           Type
-------------------------------------------------------------------------------
import qualified Control.Monad       as M
import qualified Data.Text           as T
import           Text.Megaparsec
-------------------------------------------------------------------------------

-- MainProgram -> 'program' Id 'begin' ListDefProc Block 'end'
mainProgram :: Graciela (Maybe (AST Type))
mainProgram = do
  pos <- getPosition
  panicMode parseProgram parseTokId PE.Program
  id <- panicModeId parseBegin
  panicMode parseBegin (parseProc <|> parseFunc <|> parseTokOpenBlock) PE.Begin


  ast  <- listDefProc parseTokOpenBlock parseTokOpenBlock
  lacc <- block parseEnd parseEnd
  try ( do parseEnd
           parseEOF
           return (M.liftM3 (AST.Program id pos) ast lacc (return GEmpty))
      )
      <|> do genNewError parseEOF PE.LexEnd
             return Nothing


-- Program -> Abstract Program
-- Program -> MainProgram
{- The program consists in a set of Abstract Data Types, Data Types and a main program -}
program :: Graciela (Maybe (AST Type))
program = do
  newScopeParser
  many (abstractDataType <|> dataType) -- Por ahora debe haber un programa
  mainProgram                          -- principal al final del archivo
