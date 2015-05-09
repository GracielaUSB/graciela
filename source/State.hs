module State where

import Control.Monad.State
import qualified Data.Sequence as DS
import Text.Parsec
import Control.Monad.State as ST
import Control.Monad.Identity (Identity)
import Token
import MyParseError
import MyTypeError
import SymbolTable
import Data.Text as T 

data ParserState = ParserState { synErrorList :: DS.Seq MyParseError
                               , symbolTable  :: SymbolTable
                               , typErrorList :: DS.Seq MyTypeError
                               }
      deriving(Show)

type MyParser a = ParsecT [TokenPos] () (ST.StateT (ParserState) Identity) a

initialState = ParserState { synErrorList = DS.empty, symbolTable = emptyTable, typErrorList = DS.empty }

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
