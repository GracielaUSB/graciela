{-|
Module      : Limits
Description : Limites representables
Copyright   : Graciela

Limites de los numeros enteros y flotantes del lenguaje
-}
module Limits where


-- | Retorna el maximo numero representable de los flotantes
maxDouble :: Double
maxDouble = 10 ** 308.25


-- | Retorna el maximo numero representable de los enteros
maxInteger :: Integer
maxInteger = 2 ^ (31 :: Integer) - 1


-- | Retorna el minimo numero representable de los enteros
minInteger :: Integer
minInteger = - (2 ^ (31 :: Integer))


-- | Retorna el minimo numero representable de los flotantes
minDouble :: Double
minDouble  = -1 * maxDouble
