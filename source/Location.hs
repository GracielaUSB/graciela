{-|
Module      : Location
Description : Localizacion de todo lo necesario
Copyright   : GraCieLa

Todo lo referente a la localizacion de variables, errores, palabras reservadas.
-}
module Location where

import Text.Parsec


data Location = -- | Localizacion
				Location { line   :: Int  -- ^ Linea donde se encuentra
						 , column :: Int  -- ^ Columna donde se encuentra
						 , name :: String -- ^ Nombre del Archivo
						 }
	deriving (Read, Eq) 


instance Show Location where
	show (Location line column name) ="en la línea " ++ show line ++ ", columna " ++ show column


{-|
  La función 'errorL' se encarga de imprimir la localizacion del error
-}
errorL :: Location -> String
errorL loc = "Error " ++ show loc

{-|
  La función 'getLocation' recibe un 'SourcePos' para poder
  generar la localizacion
-}
getLocation :: SourcePos -> Location
getLocation pos = Location (sourceLine pos) (sourceColumn pos) (sourceName pos)


{-|
  La función 'getFirstLoc' compara dos localidades distintas,
  para decir cual de las dos es mayor
-}
getFirstLoc x y = case compare (line x) (line y) of
				  { LT -> LT
				  ; GT -> GT
				  ; EQ -> compare (column x) (column y) 
				  } 
    