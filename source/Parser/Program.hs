module Parser.Program 
  (program 
  )where


-------------------------------------------------------------------------------
import Parser.Instructions          (block)
import Parser.Procedures            (listDefProc)
import Parser.TokenParser           
import Parser.ADT
import MyParseError                  as PE
import ParserState
import Location
import State
import Type
import Token
import AST
-------------------------------------------------------------------------------
import qualified Control.Monad       as M
import qualified Data.Text as T

import           Text.Parsec
-------------------------------------------------------------------------------

-- MainProgram -> 'program' ID 'begin' ListDefProc Block 'end'
mainProgram :: MyParser (Maybe (AST Type))
mainProgram = do 
    pos <- getPosition
    newScopeParser
    try $do M.void parseProgram
     <|> do t <- lookAhead parseID
            genNewError (return $TokId t) PE.Program
     <|> do (t,_) <- anyToken
            manyTill  anyToken (lookAhead $ parseID)
            genNewError (return t) PE.Program

    id <- try $do parseID
     <|> do (t,_) <- lookAhead anyToken
            genNewError (return t) PE.IDError
            return $ T.pack "No ID"      
     <|> do (t:_) <- manyTill anyToken (lookAhead parseBegin)
            genNewError (return $fst t) PE.IDError                           
            return $ T.pack "No ID"    

    try $do M.void $ parseBegin
     <|> do (t,_) <- lookAhead anyToken                               
            genNewError (return t) PE.Begin                           
     <|> do (t:_) <- manyTill anyToken (lookAhead (parseTokOpenBlock))  
            genNewError (return $fst t) PE.Begin                      

    try ( do
            ast  <- listDefProc parseTokOpenBlock parseTokOpenBlock
            lacc <- block parseEnd parseEnd
            try ( do parseEnd
                     parseEOF
                     return (M.liftM3 (AST.Program id (toLocation pos)) ast lacc (return (GEmpty)))
                )
                <|> do genNewError parseEOF PE.LexEnd
                       return Nothing
                 
        ) <|> do return Nothing
      

-- Program -> Abstract Program
-- Program -> MainProgram
{- The program consists in a set of Abstract Data Types, Data Types and a main program -}
program :: MyParser (Maybe (AST Type))
program = do  many (abstractDataType <|> dataType) -- Por ahora debe haber un programa 
              mainProgram                          -- principal al final del archivo



