module Parser.Program 
  (program 
  )where


-------------------------------------------------------------------------------
import Parser.Procedures
import Parser.Assertions
import Parser.Expression
import Parser.Declarations
import Parser.Instructions
import MyParseError                  as PE
import ParserState
import TokenParser
import Location
import State
import Type
import AST
-------------------------------------------------------------------------------
import qualified Control.Monad       as M
import           Text.Parsec
-------------------------------------------------------------------------------


program :: MyParser (Maybe (AST Type))
program = do pos <- getPosition
             newScopeParser
             try ( do parseProgram
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





