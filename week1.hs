module Week1 where

toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigits n
 | n <= 0    = []
 | n < 10    = [n]
 | otherwise =  toDigits (div n 10) ++ [mod n 10]

toDigitsRev n
 | n <= 0    = []
 | n < 10    = [n]
 | otherwise = mod n 10 : toDigitsRev (div n 10)


