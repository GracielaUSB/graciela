module State where

import Control.Monad.Identity (Identity)
import qualified Data.Sequence as DS
import Control.Monad.State     as ST
import Data.Text               as T   hiding (foldl) 
import Data.Foldable                  hiding (foldl)
import MyParseError
import Data.Monoid
import Text.Parsec
import MyTypeError
import SymbolTable
import Contents
import Location
import Token
import Type


data ParserState = 
        ParserState { synErrorList     :: DS.Seq MyParseError
                    , symbolTable      :: SymbolTable
                    , sTableErrorList  :: DS.Seq MyTypeError
                    }
      deriving(Show)

type MyParser a = ParsecT [TokenPos] () (ST.StateT (ParserState) Identity) a

initialState = ParserState { synErrorList = DS.empty, symbolTable = emptyTable, sTableErrorList = DS.empty }

addTypeError :: MyTypeError -> ParserState -> ParserState
addTypeError err ps = ps { sTableErrorList = (sTableErrorList ps) DS.|> err }
                                      
addParsingError :: MyParseError -> ParserState -> ParserState
addParsingError e ps = ps { synErrorList = (synErrorList ps) DS.|> e }

addNewSymbol :: T.Text -> (Contents SymbolTable) -> ParserState -> ParserState
addNewSymbol id c ps = case addSymbol id c (symbolTable ps) of
                        { Left con -> ps { sTableErrorList = (sTableErrorList ps) DS.|> (RepSymbolError id (getContentLoc con) (getContentLoc c)) }
                        ; Right sb -> ps { symbolTable  = sb }
                        }

newScopeState :: ParserState -> ParserState
newScopeState st = ParserState { synErrorList = synErrorList st
                               , symbolTable  = enterScope (symbolTable st)
                               , sTableErrorList = sTableErrorList st
                               } 

exitScopeState :: ParserState -> ParserState
exitScopeState st = case exitScope (symbolTable st) of
                    { Just sbtl -> ParserState { synErrorList = synErrorList st
                                               , symbolTable  = sbtl
                                               , sTableErrorList = sTableErrorList st 
                                               }
                    ; Nothing   -> addParsingError (ScopesError) st
                    }
getScopeState :: ParserState -> Int
getScopeState st = getScope $ symbolTable st

lookUpVarState :: T.Text -> SymbolTable -> Maybe (Contents SymbolTable)
lookUpVarState id sb = checkSymbol id sb


drawState st = case (DS.null $ synErrorList st) && (DS.null $ sTableErrorList st) of
               { True  -> "\nTABLA DE SIMBOLOS \n" ++ (show $ symbolTable st) 
               ; False -> "\n\n" ++ (drawError $ synErrorList st) ++ (drawError $ sTableErrorList st)
               }


drawError list = case (DS.null list) of
                 { True  -> ""
                 ; False -> foldl (\acc i -> acc `mappend` show i `mappend` "\n") "\n\n" (toList list)
                 }
