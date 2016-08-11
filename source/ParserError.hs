{-|
Module      : ParserError
Description : Recuperacion de errores
Copyright   : Graciela

Incluye lo referente al almacenamiento de las variables
en la tabla de símbolos, mientras se esta realizando el análisis sintáctico.
-}
module ParserError where

import           Graciela (Graciela)
import           Parser.Token (anyToken)
import           Token (Token)

import           Control.Monad (void)
import           Prelude hiding (until)
import           Text.Megaparsec (manyTill, lookAhead, (<|>), eof)



