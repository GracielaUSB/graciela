module State where

import Control.Monad.Identity (Identity)
import qualified Data.Sequence as DS
import Control.Monad.State     as ST
import Data.Text               as T 
import MyParseError
import Text.Parsec
import MyTypeError
import SymbolTable
import Contents
import Location
import Token

data ParserState = ParserState { synErrorList :: DS.Seq MyParseError
                               , symbolTable  :: SymbolTable
                               , typErrorList :: DS.Seq MyTypeError
                               }
      deriving(Show)

type MyParser a = ParsecT [TokenPos] () (ST.StateT (ParserState) Identity) a

initialState = ParserState { synErrorList = DS.empty, symbolTable = emptyTable, typErrorList = DS.empty }

addTypeError :: MyTypeError -> ParserState -> ParserState
addTypeError err ps = ParserState { synErrorList = (synErrorList ps)
                                  , symbolTable  = (symbolTable ps)
                                  , typErrorList = (typErrorList ps) DS.|> err
                                  }
                                      
addParsingError :: MyParseError -> ParserState -> ParserState
addParsingError e ps = ParserState { synErrorList = (synErrorList ps) DS.|> e
                                   , symbolTable = symbolTable ps
                                   , typErrorList = typErrorList ps
                                   }

addNewSymbol :: T.Text -> Contents -> ParserState -> ParserState
addNewSymbol id c ps = case addSymbol id c (symbolTable ps) of
                        { Left con -> ParserState { synErrorList = (synErrorList ps)
                                                  , symbolTable  = (symbolTable ps)
                                                  , typErrorList = (typErrorList ps) DS.|> (RepSymbolError id (symbolLoc con) (symbolLoc c))
                                                  }
                        ; Right sb -> ParserState { synErrorList = (synErrorList ps)
                                                  , symbolTable  = sb
                                                  , typErrorList = (typErrorList ps)
                                                  }
                        }

newScopeState :: ParserState -> ParserState
newScopeState st = ParserState { synErrorList = synErrorList st
                               , symbolTable  = enterScope (symbolTable st)
                               , typErrorList = typErrorList st
                               } 

exitScopeState :: ParserState -> ParserState
exitScopeState st = case exitScope (symbolTable st) of
                    { Just sbtl -> ParserState { synErrorList = synErrorList st
                                               , symbolTable  = sbtl
                                               , typErrorList = typErrorList st 
                                               }
                    ; Nothing   -> addParsingError (ScopesError) st
                    }

