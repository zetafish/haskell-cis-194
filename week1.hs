-- Exercise 1
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

-- Exercise 2
dub :: [Integer] -> [Integer]
dub []         = []
dub (x:[])     = [x]
dub (x:(y:zs)) = [x,(y*2)] ++ dub zs

doubleEveryOther xs = reverse (dub (reverse xs))

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate n = 0 == (sumDigits (doubleEveryOther (toDigits n))) `mod` 10

-- Exercise 5
type Peg  = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)
