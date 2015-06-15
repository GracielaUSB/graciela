module Print where

import Data.Monoid
import Location


space :: Char
space = ' '


putSpaces :: Int -> String
putSpaces   level = take level (repeat space)


putSpacesLn :: Int -> String
putSpacesLn level = "\n" `mappend` take level (repeat space)


putLocation :: Location -> String
putLocation location = " --- en el " `mappend` show location


putLocationLn :: Location -> String
putLocationLn location = " --- en el " `mappend` show location `mappend` "\n"
