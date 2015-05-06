module Location where


data Location = Location { line :: Int, column :: Int, name :: String }
  deriving (Read, Eq) 


instance Show Location where
	show (Location line column name) ="Archivo " ++show name++ ": línea " ++show line++ ", columna " ++show column