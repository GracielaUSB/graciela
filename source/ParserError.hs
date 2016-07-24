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


-- -- | Se encarga de descartar tokens hasta llegar a algun follow de la regla.
-- cleanEntry :: Graciela Token
--            -> Graciela (Token, SourcePos)
-- cleanEntry laset =
--   do pos <- getPosition
--      e   <- lookAhead laset <|> eof <|> anyToken
--      panicMode laset
--      return (e, pos)


-- | Se encarga de ignorar tokens hasta encontrar 'until'
panicMode :: Graciela Token -> Graciela [Token]
panicMode until = manyTill anyToken (void (lookAhead until) <|> eof)
