module State where

import Control.Monad.State
import qualified Data.Sequence as DS
import Text.Parsec
import Control.Monad.State as ST
import Control.Monad.Identity (Identity)
import Token
import MyParseError

data ParserState = ParserState { errorList :: DS.Seq MyParseError }
      deriving(Show)

type MyParser a = ParsecT [TokenPos] () (ST.StateT (ParserState) Identity) a

initialState = ParserState { errorList = DS.empty }

addError :: MyParseError -> ParserState -> ParserState
addError e ps = ParserState { errorList = (errorList ps) DS.|> e }
