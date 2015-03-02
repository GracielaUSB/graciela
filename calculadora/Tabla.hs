-- Tabla 

import qualified Data.Tree as Tree
import qualified Data.Text as T
import qualified Data.Map  as M


type Linea   =  Integer 
type Columna =  Integer 
type Offset  =  Integer

data Struct = Struct Linea Columna Offset deriving (Read, Eq, Show, Ord)

type Diccionario = M.Map T.Text Struct        

data TablaSimbolos = Tabla { actual :: Diccionario, padre :: TablaSimbolos, hijos :: [TablaSimbolos] } deriving (Read, Eq, Show, Ord)



setElemento :: TablaSimbolos -> String -> Linea -> Columna -> Offset -> TablaSimbolos 
setElemento tabla valor linea columna offset = 
        let newActual = M.insert (T.pack valor) (Struct linea columna offset) (actual tabla)
            in Tabla newActual (padre tabla) (hijos tabla)


setHijo :: TablaSimbolos -> TablaSimbolos
setHijo tabla = Tabla (M.empty) (padre tabla) []


enterScope :: TablaSimbolos -> TablaSimbolos -> TablaSimbolos
enterScope tabla hijo = Tabla (actual tabla) (padre tabla) $ hijo:(hijos tabla)


exitScope :: TablaSimbolos -> TablaSimbolos
exitScope tabla = padre tabla

