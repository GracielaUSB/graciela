module State where
--------------------------------------------------------------------------------
import           Contents
import           Data.Monoid
import           Location
import           MyParseError           as P
import           MyTypeError            as T
import           SymbolTable
import           Text.Parsec
import           Token
--------------------------------------------------------------------------------
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (StateT)
import           Data.Foldable          (toList)
import           Data.Function          (on)
import           Data.Sequence          (Seq, (|>))
import qualified Data.Sequence          as Seq (empty, null, sortBy)
import qualified Data.Set               as Set (Set, empty, insert)
import           Data.Text              (Text)
--------------------------------------------------------------------------------

type MyParser a = ParsecT [TokenPos] () (StateT ParserState Identity) a


data ParserState = ParserState
    { synErrorList    :: Seq MyParseError
    , symbolTable     :: SymbolTable
    , sTableErrorList :: Seq MyTypeError
    , filesToRead     :: Set.Set String
    }
    deriving(Show)


initialState :: ParserState
initialState = ParserState
    { synErrorList    = Seq.empty
    , symbolTable     = emptyTable
    , sTableErrorList = Seq.empty
    , filesToRead     = Set.empty
    }


addFileToRead :: String -> ParserState -> ParserState
addFileToRead file ps =
    ps { filesToRead = Set.insert file $ filesToRead ps }


addTypeError :: MyTypeError -> ParserState -> ParserState
addTypeError err ps =
    ps { sTableErrorList = sTableErrorList ps |> err }


addParsingError :: MyParseError -> ParserState -> ParserState
addParsingError e ps =
    ps { synErrorList = synErrorList ps |> e }


addNewSymbol :: Text -> Contents SymbolTable -> ParserState -> ParserState
addNewSymbol sym c ps = case addSymbol sym c (symbolTable ps) of
    Left con ->
        ps { sTableErrorList =
            sTableErrorList ps |> (RepSymbolError sym `on` symbolLoc) con c }
    Right sb ->
        ps { symbolTable = sb }


initVar :: Text -> ParserState -> ParserState
initVar sym ps = ps { symbolTable = initSymbol sym (symbolTable ps) }


newScopeState :: ParserState -> ParserState
newScopeState st = st { symbolTable = enterScope (symbolTable st) }


exitScopeState :: ParserState -> ParserState
exitScopeState st = case exitScope (symbolTable st) of
    Just sbtl -> st { symbolTable = sbtl }
    Nothing   -> addParsingError ScopesError st


getScopeState :: ParserState -> Int
getScopeState st = getScope $ symbolTable st


lookUpVarState :: Text -> SymbolTable -> Maybe (Contents SymbolTable)
lookUpVarState = checkSymbol


drawState :: Maybe Int -> ParserState -> String
drawState n st = if Seq.null $ synErrorList st
    then if Seq.null $ sTableErrorList st
        then "\n HUBO UN ERROR PERO LAS LISTAS ESTAN VACIAS... \n"
        else drawError . take' n . Seq.sortBy (compare `on` T.loc) . sTableErrorList $ st
    else drawError . take' n . Seq.sortBy (compare `on` P.loc) . synErrorList $ st


drawError list = if Seq.null list
    then "LISTA DE ERRORES VACIA"
    else unlines . map show . toList $ list
