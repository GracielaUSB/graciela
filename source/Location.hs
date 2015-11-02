module Location where

import Text.Parsec


data Location = Location { line :: Int, column :: Int, name :: String }
  deriving (Read, Eq) 


instance Show Location where
	show (Location line column name) ="en la línea " ++ show line ++ ", columna " ++ show column


errorL :: Location -> String
errorL loc = "Error " ++ show loc


getLocation :: SourcePos -> Location
getLocation pos = Location (sourceLine pos) (sourceColumn pos) (sourceName pos)


--getFirstLoc :: Location -> Location -> Location
getFirstLoc x y = case compare (line x) (line y) of
				  { LT -> LT
				  ; GT -> GT
				  ; EQ -> compare (column x) (column y) 
				  } 
    