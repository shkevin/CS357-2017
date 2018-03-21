{-
Name: Kevin Cox
NetID: Shkevin
-}

#!/usr/bin/env stack

import Data.List

----------------------------------------------3.1--------------------------------------------------
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Converts a non-empty list into a balance tree. 
balance :: [a] -> Tree a
balance [x] = Leaf x
balance list = Node (balance xs) (balance ys)
               where (xs,ys) = splitList list

--Returns a given list split in half
splitList :: [a] -> ([a], [a])
splitList tree = splitAt ((length tree) `div` 2) tree
---------------------------------------------------------------------------------------------------

----------------------------------------------3.2--------------------------------------------------
--Goldback Conjecture: Any even number > 2 can be written as the sum of two prime numbers.
goldbach :: Int -> [(Int, Int)]
goldbach n = [(x,y) | x <- sieve n, y <- sieve n, x + y == n]

--Test whether or not the given number is prime. This uses the Trial Division method.
testPrime :: Int -> Bool
testPrime n = length [x | x <- getDivisors n, n `mod` x == 0] <= 1

--Gets all the divisors of the given number, including 1.
getDivisors :: Int -> [Int]
getDivisors n = [x | x <- [1 .. ceiling (sqrt $ fromIntegral n)]]

--Technically not a true sieve, this is very inefficient. Starts at 2 to ignore 1 as prime.
sieve :: Int -> [Int]
sieve n = [[2..n] !! x | x <- elemIndices (True) (map testPrime [2..n])]
---------------------------------------------------------------------------------------------------

----------------------------------------------3.3--------------------------------------------------
church :: Int -> (c -> c) -> c -> c
church = undefined
---------------------------------------------------------------------------------------------------

----------------------------------------------3.4--------------------------------------------------
--S invariants: Needs to have no duplicates, and monotonically increasing order.
--P(S) invariants: Needs to have no duplicates, and order is immaterial.

--Returns the powerset of a given set S. Where the powerset is the set of all subsets of S.
powerset :: [Int] -> [[Int]]
powerset s = undefined
---------------------------------------------------------------------------------------------------