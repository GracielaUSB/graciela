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
import Type

data ParserState = ParserState { synErrorList :: DS.Seq MyParseError
                               , symbolTable  :: SymbolTable
                               , typErrorList :: DS.Seq MyTypeError
                               }
      deriving(Show)

type MyParser a = ParsecT [TokenPos] () (ST.StateT (ParserState) Identity) a

initialState = ParserState { synErrorList = DS.empty, symbolTable = emptyTable, typErrorList = DS.empty }

addTypeError :: MyTypeError -> ParserState -> ParserState
addTypeError err ps = ps { typErrorList = (typErrorList ps) DS.|> err }
                                      
addParsingError :: MyParseError -> ParserState -> ParserState
addParsingError e ps = ps { synErrorList = (synErrorList ps) DS.|> e }

addNewSymbol :: T.Text -> Contents -> ParserState -> ParserState
addNewSymbol id c ps = case addSymbol id c (symbolTable ps) of
                        { Left con -> ps { typErrorList = (typErrorList ps) DS.|> (RepSymbolError id (symbolLoc con) (symbolLoc c)) }
                        ; Right sb -> ps { symbolTable  = sb }
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
getScopeState :: ParserState -> Int
getScopeState st = getScope $ symbolTable st

lookUpVarState :: T.Text -> SymbolTable -> Maybe Type
lookUpVarState id sb = fmap symbolType $ checkSymbol id sb
