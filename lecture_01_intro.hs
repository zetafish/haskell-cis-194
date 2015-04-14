foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0              = 0
  | n `mod` 17 == 2    = -43
  | otherwise          = n + 3

isEven :: Integer -> Bool
isEven n
  | n `mod` 2 == 0 = True
  | otherwise      = False

p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

-- Function with multiple arguments
f :: Int -> Int -> Int -> Int
f x y z = x + y + z

-- Lists
ex18 = 1 : []

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []
sumEveryTwo (x:[])     = [x]
sumEveryTwo (x:(y:zs)) = (x+y) : sumEveryTwo zs

-- And now for the homework
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0     = []
  | otherwise  = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
