module Parser.Program 
  (program 
  )where


-------------------------------------------------------------------------------
import Parser.Instructions          (block)
import Parser.Procedures            (listDefProc)
import Parser.TokenParser           
import Parser.TAD
import MyParseError                  as PE
import ParserState
import Location
import State
import Type
import AST
-------------------------------------------------------------------------------
import qualified Control.Monad       as M
import           Text.Parsec
-------------------------------------------------------------------------------

-- MainProgram -> 'program' ID 'begin' ListDefProc Block 'end'
mainProgram :: MyParser (Maybe (AST Type))
mainProgram = do 
    pos <- getPosition
    newScopeParser
    try ( do  parseProgram
              try ( do id  <- parseID
                       try ( do parseBegin
                                ast  <- listDefProc parseTokOpenBlock parseTokOpenBlock
                                lacc <- block parseEnd parseEnd
                                try ( do parseEnd
                                         parseEOF
                                         return (M.liftM3 (AST.Program id (toLocation pos)) ast lacc (return (GEmpty)))
                                    )
                                    <|> do genNewError parseEOF PE.LexEnd
                                           return Nothing
                           )
                           <|> do genNewError parseEOF PE.Begin
                                  return Nothing
                  )
                  <|> do genNewError parseEOF PE.IDError
                         return Nothing
       )
      <|> do genNewError parseEOF PE.Program
             return Nothing

-- Program -> Abstract Program
-- Program -> Program
program :: MyParser (Maybe (AST Type))
program =   do abstract
               program          -- Por ahora el programa principial  
        <|> do mainProgram      -- debe estar en el fondo del archivo
               
        <|> return Nothing

