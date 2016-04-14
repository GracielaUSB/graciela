{-|
Module      : Limits
Description : Limites representables
Copyright   : GraCieLa

Limites de los numeros enteros y flotantes del lenguaje
-}
module Limits where


{-|
  La función 'maxDouble' maximo numero representable de los flotantes
-}
maxDouble :: Double
maxDouble = 10 ** 308.25


{-|
  La función 'maxInteger' maximo numero representable de los enteros
-}
maxInteger :: Integer
maxInteger = 2 ^ 31 - 1


{-|
  La función 'minInteger' minimo numero representable de los enteros
-}
minInteger :: Integer
minInteger = - (2 ^ 31)


{-|
  La función 'minDouble' minimo numero representable de los flotantes
-}
minDouble :: Double
minDouble  = -1 * maxDouble
