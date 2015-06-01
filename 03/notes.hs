module Notes_03 where

-- Recursion patterns
data IntList = Empty | Cons Int IntList
             deriving Show

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

addOne x = x+1
square x = x*x
exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList f Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList f Empty = Empty
filterIntList f (Cons x xs) =
  if f x
  then Cons x (filterIntList f xs)
  else filterIntList f xs

-- Polymorphism
data List t = E | C t (List t) deriving Show

lst1 :: List Int
lst1 =  C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs

mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x
