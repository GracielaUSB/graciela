module State where

import qualified Data.Sequence as DS
import qualified Data.Set      as SET
import Control.Monad.Identity        (Identity)
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

type MyParser a = ParsecT [TokenPos] () (ST.StateT (ParserState) Identity) a


data ParserState = ParserState { synErrorList     :: DS.Seq MyParseError
                               , symbolTable      :: SymbolTable
                               , sTableErrorList  :: DS.Seq MyTypeError
                               , filesToRead      :: SET.Set String
                               }
      deriving(Show)


initialState :: ParserState
initialState = ParserState { synErrorList = DS.empty, symbolTable = emptyTable, sTableErrorList = DS.empty, filesToRead = SET.empty }


addFileToRead :: String -> ParserState -> ParserState
addFileToRead file ps = ps { filesToRead = SET.insert file $ filesToRead ps }


addTypeError :: MyTypeError -> ParserState -> ParserState
addTypeError err ps = ps { sTableErrorList = (sTableErrorList ps) DS.|> err }
      

addParsingError :: MyParseError -> ParserState -> ParserState
addParsingError e ps = ps { synErrorList = (synErrorList ps) DS.|> e }


addNewSymbol :: T.Text -> (Contents SymbolTable) -> ParserState -> ParserState
addNewSymbol id c ps = case addSymbol id c (symbolTable ps) of
                        { Left con -> ps { sTableErrorList = (sTableErrorList ps) DS.|> 
                                           (RepSymbolError id (symbolLoc con) (symbolLoc c)) }
                        ; Right sb -> ps { symbolTable = sb }
                        }


initVar :: T.Text -> ParserState -> ParserState
initVar id ps = ps { symbolTable = initSymbol id (symbolTable ps) }


newScopeState :: ParserState -> ParserState
newScopeState st = st { symbolTable     = enterScope (symbolTable st) } 


exitScopeState :: ParserState -> ParserState
exitScopeState st = case exitScope (symbolTable st) of
                    { Just sbtl -> st { symbolTable = sbtl }
                    ; Nothing   -> addParsingError ScopesError st
                    }


getScopeState :: ParserState -> Int
getScopeState st = getScope $ symbolTable st


lookUpVarState :: T.Text -> SymbolTable -> Maybe (Contents SymbolTable)
lookUpVarState id sb = checkSymbol id sb


drawState :: ParserState -> String
drawState st = case (DS.null $ synErrorList st) of
               { False  -> drawError $ DS.sortBy checkErrorPosP (synErrorList st)
               ; True   -> case (DS.null $ sTableErrorList st) of
                          { True  ->  "\n HUBO UN ERROR PERO LAS LISTAS ESTAN VACIAS... \n"
                                      --TABLA DE SIMBOLOS \n" ++ (show $ symbolTable st) 
                          ; False -> drawError $ DS.sortBy checkErrorPosT (sTableErrorList st)
                          }
               }


drawError list = case (DS.null list) of
                 { True  -> "LISTA DE ERRORES VACIA"
                 ; False -> foldl (\acc i -> acc `mappend` show i `mappend` "\n") "\n" (toList list)
                 }



drawState2 :: ParserState -> String
drawState2 st = case (DS.null $ synErrorList st) of
               { False  -> drawError $ DS.take 1 $ DS.sortBy checkErrorPosP (synErrorList st)
               ; True   -> case (DS.null $ sTableErrorList st) of
                          { True  ->  "\n HUBO UN ERROR PERO LAS LISTAS ESTAN VACIAS... \n"
                                      --TABLA DE SIMBOLOS \n" ++ (show $ symbolTable st) 
                          ; False -> drawError $ DS.take 1 $ DS.sortBy checkErrorPosT (sTableErrorList st)
                          }
               }
