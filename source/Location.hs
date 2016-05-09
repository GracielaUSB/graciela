{-|
Module      : Location
Description : Localizacion de todo lo necesario
Copyright   : GraCieLa

Posee todo lo referente a la localizacion de variables, errores, palabras reservadas.
-}
module Location where
--------------------------------------------------------------------------------
import Text.Parsec (SourcePos, sourceLine, sourceColumn, sourceName)
--------------------------------------------------------------------------------

-- | Es el tipo de dato para la localizacion de una variable, error, etc.
data Location = Location
    { line   :: Int    -- ^ Linea donde se encuentra
    , column :: Int    -- ^ Columna donde se encuentra
    , name   :: String -- ^ Nombre del Archivo
    }
    deriving (Read, Eq, Ord)


instance Show Location where
    show (Location l c _) =
        "en la línea " ++ show l ++ ", columna " ++ show c


-- | Se encarga de imprimir la localizacion del error.
errorL :: Location -> String
errorL loc = "Error " ++ show loc


-- | Recibe un 'SourcePos' para poder generar la localizacion.
toLocation :: SourcePos -> Location
toLocation pos =
    Location (sourceLine pos) (sourceColumn pos) (sourceName pos)
