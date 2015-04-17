-- Tabla 

import qualified Data.Tree           as Tree
import qualified Data.Text           as T
import qualified Data.Map            as M
import qualified Data.Either.Unwrap  as Unwrap
import qualified Data.Maybe          as Maybe
import Type

type Line   =  Integer 
type Column =  Integer 
type Offset =  Integer

data Contents = Contents Line Column Type deriving (Read, Eq, Show)

type Diccionario = M.Map T.Text Contents

data SymbolTable = Tabla { actual :: Diccionario, padre :: Maybe SymbolTable, hijos :: [SymbolTable] }
      deriving (Show)

addSymbol :: T.Text -> Contents -> SymbolTable ->  Either String SymbolTable
addSymbol valor content tabla =
          if checkSymbol valor tabla then Left "Error: El símbolo ya se encontraba en la tabla de símbolos" 
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
exitScope tabla = let f table = Tabla (actual table) (padre table) (tabla : (hijos table))
                      in fmap f (padre tabla)

-- Casos de prueba

t0 = emptyTable

t1 = addSymbol (T.pack "foo") (Contents 1 2 MyInt) t0

t2 = addSymbol (T.pack "bar") (Contents 3 4 MyInt) (Unwrap.fromRight t1)

t3 = enterScope (Unwrap.fromRight t2)

t4 = addSymbol (T.pack "foo fighters") (Contents 7 8 MyFloat) (t3)

t5 = enterScope (Unwrap.fromRight t4)

t6 = addSymbol (T.pack "metallica") (Contents 7 8 MyChar) (t5)

t7 = exitScope (Unwrap.fromRight t6)

t8 = exitScope (Maybe.fromJust t7)

t9 = exitScope (Maybe.fromJust t8)
