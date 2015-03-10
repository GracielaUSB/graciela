-- Tabla 

import qualified Data.Tree as Tree
import qualified Data.Text as T
import qualified Data.Map  as M
import qualified Data.Either.Unwrap as U

type Line   =  Integer 
type Column =  Integer 
type Offset =  Integer

data Contents = Contents Line Column Offset deriving (Read, Eq, Show, Ord)

type Diccionario = M.Map T.Text Contents

data SymbolTable = Tabla { actual :: Diccionario, padre :: Maybe SymbolTable, hijos :: [SymbolTable] }
      deriving (Read, Eq, Show, Ord)

addSymbol :: T.Text -> Contents -> SymbolTable ->  Either String SymbolTable
addSymbol valor content tabla =
          if checkSymbol valor tabla then Left "Error" 
          else  let newActual = M.insert (valor) (content) (actual tabla) in
                    Right $ Tabla newActual (padre tabla) (hijos tabla)

checkSymbol valor tabla = let dic = actual tabla in
                            if M.member valor dic then True
                            else case padre tabla of
                                   { Nothing   -> False
                                   ; Just sup  -> checkSymbol valor sup
                                   }

flatten symbolTable = (actual symbolTable) : concatMap flatten (hijos symbolTable)

emptyTable :: SymbolTable
emptyTable = Tabla (M.empty) (Nothing) []

enterScope :: SymbolTable -> SymbolTable
enterScope tabla = Tabla (M.empty) (Just tabla) ([])

exitScope :: SymbolTable -> Maybe SymbolTable
exitScope tabla = let pTable = padre tabla in
                    case pTable of
                    { Just table -> Just $ Tabla (actual table) (padre table) (tabla : (hijos table))
                    ; Nothing    -> Nothing
                    }

tablaPadre = Tabla (M.fromList ([(T.pack "s1", Contents 5 5 5), (T.pack "s2", Contents 6 6 6), (T.pack "s3", Contents 6 6 6)])) (Nothing) ([])
tablaHijo = Tabla (M.fromList ([(T.pack "s4", Contents 5 5 5), (T.pack "s5", Contents 6 6 6), (T.pack "s6", Contents 6 6 6)])) (Nothing) ([])

cont = Contents 8 7 8
