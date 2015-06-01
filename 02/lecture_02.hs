-- Enumeratioon types

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
           deriving Show

shoe :: Thing
shoe = Shoe

-- We can make list of enumeration
listOfThings :: [Thing]
listOfThings = [Shoe, King, Cabbage]

-- Do some pattern matching
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

-- Function clauses are tried from top to bottom so
-- we could also do so:
isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _    = True

-- Beyond enumerations
data FailableDouble = Failure | OK Double deriving Show

ex01 = Failure
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- Data constructors with multiple args
data Person = Person String Int Thing
            deriving Show

brent :: Person
brent = Person "Brent" 31 Shoe

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

-- Pattern maching
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- Nested patterns
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", nice one"
checkFav (Person n _ _)          = n ++ " so lame"

-- Case expressions
ex03 = case "Hello" of
        []      -> 3
        ('H':s) -> length s
        _       -> 7


-- Recursive data types
data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

data Tree = Leaf Char
          | Node Tree Int Tree
          deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
