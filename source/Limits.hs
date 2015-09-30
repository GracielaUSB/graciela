module Limits where


maxDouble :: Double
maxDouble = 10 ** 308.25

maxInteger :: Integer
maxInteger = 2 ^ 31 - 1

minInteger :: Integer
minInteger = - (2 ^ 31)

minDouble :: Double
minDouble  = -1 * maxDouble
