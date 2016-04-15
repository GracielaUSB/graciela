{-|
Module      : Location
Description : Localizacion de todo lo necesario
Copyright   : GraCieLa

Posee todo lo referente a la localizacion de variables, errores, palabras reservadas.
-}
module Location where

import Text.Parsec


-- | Es el tipo de dato para la localizacion de una variable, error, etc.
data Location =  Location { line :: Int    -- ^ Linea donde se encuentra
                          , column :: Int  -- ^ Columna donde se encuentra
                          , name :: String -- ^ Nombre del Archivo
                          }
    deriving (Read, Eq) 


-- | Instancia 'Show' para la localizaciones.
instance Show Location where
    show (Location line column name) ="en la línea " ++ show line ++ ", columna " ++ show column


-- | Se encarga de imprimir la localizacion del error.
errorL :: Location -> String
errorL loc = "Error " ++ show loc


-- | Recibe un 'SourcePos' para poder generar la localizacion.
getLocation :: SourcePos -> Location
getLocation pos = Location (sourceLine pos) (sourceColumn pos) (sourceName pos)


-- | Compara dos localidades distintas, para decir cual de las dos es mayor.
getFirstLoc :: Location -> Location -> Ordering
getFirstLoc x y = case compare (line x) (line y) of
    { LT -> LT
    ; GT -> GT
    ; EQ -> compare (column x) (column y) 
    } 
    