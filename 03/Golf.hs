module Golf where

import Data.Maybe
import Data.List.Split

lst = [1,2,3,4,5]


-- Exercise 1
skips :: [a] -> [[a]]
skips xs = map f [1 .. length xs]
           where f n = (map last
                        $ filter (\s -> length s == n)
                        (chunksOf n xs))

-- Exercise 2
maybeMax :: Int -> [Int] -> Maybe Int
maybeMax n xs =
  let s = length xs in
  if (n > 0   && (xs!!n > xs!!(n-1))) &&
     (n < s-1 && (xs!!n > xs!!(n+1)))
  then Just (xs!!n)
  else Nothing

localMaxima :: [Int] -> [Int]
localMaxima xs = mapMaybe (\n -> maybeMax n xs) [0..length xs-1]
