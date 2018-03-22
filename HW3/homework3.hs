{-
Name: Kevin Cox
NetID: Shkevin
-}

#!/usr/bin/env stack

import Data.List

----------------------------------------------3.1--------------------------------------------------
data Tree a = Leaf a | Node (Tree a) (Tree a)

--Converts a non-empty list into a balance tree.
balance :: [a] -> Tree a
balance [x] = Leaf x
balance list = Node (balance xs) (balance ys)
               where (xs,ys) = splitList list

--Returns a given list split in half
splitList :: [a] -> ([a], [a])
splitList tree = splitAt (div (length tree) 2) tree
---------------------------------------------------------------------------------------------------

----------------------------------------------3.2--------------------------------------------------
--Goldback Conjecture: Any even number > 2 can be written as the sum of two prime numbers.
goldbach :: Int -> [(Int, Int)]
goldbach n = [(x,y) | x <- sieve n, y <- sieve n, x + y == n]

--Test whether or not the given number is prime. This uses the Trial Division method.
testPrime :: Int -> Bool
testPrime n = length [x | x <- [y | y <- [1 .. ceiling (sqrt $ fromIntegral n)], n `mod` y == 0] ] <= 1

--Technically not a true sieve, this is very inefficient. Starts at 2 to ignore 1 as prime.
sieve :: Int -> [Int]
sieve n = [[2..n] !! x | x <- elemIndices (True) (map (testPrime) [2..n])]
---------------------------------------------------------------------------------------------------

----------------------------------------------3.3--------------------------------------------------
--takes an int n as its argument and returns a function which composes any unary function n times.
church :: Int -> (c -> c) -> c -> c
church 0 = \x y -> y
church n = \x y -> x $ church (n + (-1)) x y
---------------------------------------------------------------------------------------------------

----------------------------------------------3.4--------------------------------------------------
--S invariants: Needs to have no duplicates, and monotonically increasing order.
--P(S) invariants: Needs to have no duplicates, and order is immaterial.

--Returns the powerset of a given set S. Where the powerset is the set of all subsets of S.
powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs
---------------------------------------------------------------------------------------------------

----------------------------------------------3.5--------------------------------------------------
makeCommand :: [[(Double,Double)]] -> String
makeCommand = undefined
---------------------------------------------------------------------------------------------------

----------------------------------------------3.6--------------------------------------------------
--Need to ensure that adding prime to this doesn't matter for grading
data T = Leaf' | Node' T T
data P = GoLeft P | GoRight P | This

allpaths :: T -> [P]
allpaths = undefined
---------------------------------------------------------------------------------------------------

----------------------------------------------3.7--------------------------------------------------
type Expr = [[Int]]

eval :: (Int -> Bool) -> Expr -> Bool
eval = undefined

satisfiable :: Expr -> Bool
satisfiable = undefined
---------------------------------------------------------------------------------------------------

----------------------------------------------3.8--------------------------------------------------
data Field = B | R | G deriving (Eq, Ord, Show)
type Board = [Field]

strategyForGreen :: Board -> Int
strategyForGreen = undefined

strategyForRed :: Board -> Int
strategyForRed = undefined
---------------------------------------------------------------------------------------------------
