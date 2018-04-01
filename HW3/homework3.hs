{-
Name: Kevin Cox
NetID: Shkevin
-}

#!/usr/bin/env stack

module Homework3
( Tree(..),
  balance,
  goldbach,
  church,
  powerset,
  makeCommand,
  T(..),
  P(..),
  allpaths,
  Expr,
  eval,
  satisfiable
) where

import Data.List

----------------------------------------------3.1--------------------------------------------------
data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Eq, Show)

{-
  * PARAMETERS: List of generic type.
  * FUNCTION: Converts a non-empty list into a balance tree.
  * RETURNS: Tree of generic type after balancing.
-}
balance :: [a] -> Tree a
balance [x] = LeafT x
balance list = NodeT (balance xs) (balance ys)
               where (xs,ys) = splitList list

{-
  * PARAMETERS: List of generic type.
  * FUNCTION: Splits the given list into half.
  * RETURNS: Tuple of split list.
-}
splitList :: [a] -> ([a], [a])
splitList tree = splitAt (div (length tree) 2) tree
---------------------------------------------------------------------------------------------------

----------------------------------------------3.2--------------------------------------------------
{-
  * PARAMETERS: Size of prime list.
  * FUNCTION: Goldback Conjecture: Any even number > 2 can be written as the sum of two 
              prime numbers.
  * RETURNS: List of tuples containing primes that add to given number.
-}
goldbach :: Int -> [(Int, Int)]
goldbach n = removeDuplTuples [(x,y) | x <- sieve n, y <- sieve n, x + y == n]

{-
  * PARAMETERS: Int to be tested.
  * FUNCTION: Test whether or not the given number is prime. This uses the Trial 
              Division method.
  * RETURNS: If Int is prime or not.
-}
testPrime :: Int -> Bool
testPrime n = null [x | x <- [2 .. ceiling (sqrt $ fromIntegral n)], n `mod` x == 0]

{-
  * PARAMETERS: Prime number to generate up to.
  * FUNCTION: Technically not a true sieve, this is very inefficient. Starts at 2 to ignores 
              1 as prime.
  * RETURNS: List of primes up to given number.
-}
sieve :: Int -> [Int]
sieve n = [[2..n] !! x | x <- elemIndices (True) (map (testPrime) [2..n])]


{-
  * PARAMETERS: 
  * FUNCTION: 
  * RETURNS: 
-}
removeDuplTuples :: Eq a => [(a,a)] -> [(a,a)]
removeDuplTuples = nubBy symEq

{-
  * PARAMETERS: 
  * FUNCTION: 
  * RETURNS: 
-}
symEq :: Eq a => (a,a) -> (a,a) -> Bool
symEq (x,y) (u,v) = (x == u && y == v) || (x == v && y == u)
---------------------------------------------------------------------------------------------------

----------------------------------------------3.3--------------------------------------------------
{-
  * PARAMETERS: Number of times to compose the given function.
  * FUNCTION: takes an int n as its argument and returns a function which composes any 
              unary function n times.
  * RETURNS: Function composition of given function.
-}
church :: Int -> (c -> c) -> c -> c
church n f = foldr (.) id (replicate n f)
---------------------------------------------------------------------------------------------------

----------------------------------------------3.4--------------------------------------------------
--S invariants: Needs to have no duplicates, and monotonically increasing order.
--P(S) invariants: Needs to have no duplicates, and order is immaterial.

{-
  * PARAMETERS: Set user wants to get powerset of.
  * FUNCTION: Returns the powerset of a given set S. Where the powerset is the all subsets of S.
  * RETURNS: Powerset of the given set.
-}
powerset :: [Int] -> [[Int]]
powerset (x:xs) = subsequences (x:xs)
---------------------------------------------------------------------------------------------------

----------------------------------------------3.5--------------------------------------------------
example :: [[(Double, Double)]]
example = [[(100.0,100.0),(100.0,200.0),(200.0,100.0)],
  [(150.0,150.0),(150.0,200.0),(200.0,200.0),(200.0,150.0)]]

header = "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: "
footer = "showpage\n%%EOF"

{-
  * PARAMETERS: Doubley nested list of points for the polygon.
  * FUNCTION: The result returned by makeCommand is a Haskell value of type String, which
              contains PostScript commands for drawing the given polgygon.
  * RETURNS: PostScript string.
-}
makeCommand :: [[(Double,Double)]] -> String
makeCommand xs = header ++ printHeader xs ++ "\n\n" ++ makeCommandHelper xs

{-
  * PARAMETERS: List of tuples of Doubles of points.
  * FUNCTION: Builds up the necessary PostScript string of a list of tuple points
  * RETURNS: PostScript string of commands.
-}
printListOfTuples :: [(Double, Double)] -> String
printListOfTuples (x:xs) = concat (intersperse "" str) ++ "closepath\nstroke\n\n"
                           where y:ys = map (\(a,b) -> (show a) ++ " " ++ (show b)) (x:xs)
                                 str = (y ++ " moveto\n"):(map (++" lineto\n") ys)

{-
  * PARAMETERS: List of list of tuples of Doubles of points.
  * FUNCTION: Helper function for makeCommand that iterates through the given
              List of lists.
  * RETURNS: Built up string of the list of lists.
-}
makeCommandHelper :: [[(Double,Double)]] -> String
makeCommandHelper [] = footer
makeCommandHelper (x:xs) = (printListOfTuples x) ++ makeCommandHelper xs

{-
  * PARAMETERS: List of list of tuples of Doubles of points.
  * FUNCTION: Gets the minimum and maximum of the list of lists.
  * RETURNS: String of minimum and maximum.
-}
printHeader :: [[(Double, Double)]] -> String
printHeader (x:xs) = ((show . fst) min ++ " " ++ (show . snd) min) ++ " " ++ ((show . fst) max ++ " " ++ (show . snd) max)
                    where min = (minimum . minimum) (x:xs)
                          max = (maximum . maximum) (x:xs)
---------------------------------------------------------------------------------------------------

----------------------------------------------3.6--------------------------------------------------
data T = Leaf | Node T T deriving (Eq, Show)
data P = GoLeft P | GoRight P | This deriving (Eq, Show)

{-
  * PARAMETERS: Tree to find paths.
  * FUNCTION: Finds all paths from root to leaves in given Tree T.
  * RETURNS: List of data P, such that the list are the directions to the each path.
-}
allpaths :: T -> [P]
allpaths Leaf = [This]
allpaths (Node l r) = [This] ++ map GoLeft (allpaths l) ++ map GoRight (allpaths r)
---------------------------------------------------------------------------------------------------

----------------------------------------------3.7--------------------------------------------------
type Expr = [[Int]]

-- [[-1, 2, 4], [-2, -3]] == (¬x1 ∨ x2 ∨ x4) ∧ (¬x2 ∨ ¬x3)
{-
  * PARAMETERS: Function to test eval with, Given Expression.
  * FUNCTION: Evaluates the given List of List of Ints as a CNF.
  * RETURNS: Whether the expression evaluates to True or False.
-}
eval :: (Int -> Bool) -> Expr -> Bool 
eval f [] = True
eval f (x:xs) = (any (True==) $ flipNegative f x) && (eval f xs)

{-
  * PARAMETERS: function to test, clause inside expression
  * FUNCTION: Flips the given evaluated function if variable in clause is negative.
  * RETURNS: List of Bools after evaluated.
-}
flipNegative :: (Int -> Bool) -> [Int] -> [Bool]
flipNegative f [] = []
flipNegative f (x:xs)
                    | x < 0 = not (f $ abs x) : flipNegative f xs
                    | otherwise = (f $ abs x) : flipNegative f xs

{-
  * PARAMETERS: Expression to test.
  * FUNCTION: Checks whether the given expression can be satisfied.
  * RETURNS: If satisfiable.
-}
satisfiable :: Expr -> Bool
satisfiable xs = any (True==) $ iterateTable (permuteTheTruth $ (maximum . maximum) xs) xs

{-
  * PARAMETERS: List of list of Bools (permuations), and given expression.
  * FUNCTION: Iterates the permutations and evaluates the expression with each.
  * RETURNS: List of Bools of whether the permuations evaluates to true or false.
-}
iterateTable :: [[Bool]] -> Expr -> [Bool]
iterateTable [] _ = []
iterateTable (x:xs) expr = eval (test x) expr : iterateTable xs expr

{-
  * PARAMETERS: List of True/False, and n (index).
  * FUNCTION: Mock function to map the index to a True/False value.
  * RETURNS: Index and Bool value. Ex : 1 -> True, 2 -> False.
-}
test :: [Bool] -> Int -> Bool
test xs n = (!!) xs (n - 1)

{-
  * PARAMETERS: Int value for the size of the truth table. Will be 2^n.
  * FUNCTION: Generates truth table of specified size.
  * RETURNS: Returns the truth table as a List of List of Bools for the size specified.
-}
permuteTheTruth :: Int -> [[Bool]]
permuteTheTruth = sequence . flip replicate b
                  where b = [b | b <- [True, False]]
---------------------------------------------------------------------------------------------------

----------------------------------------------3.8--------------------------------------------------
data Field = B | R | G deriving (Eq, Ord, Show)
type Board = [Field]

-- B = unoccupied
-- R = field occupied by first player
-- G = field occupied by second player
-- Ex: BRG
--     BRG
--     BRB

strategyForGreen :: Board -> Int
strategyForGreen = undefined

strategyForRed :: Board -> Int
strategyForRed = undefined

--Takes in a Board of fields and maximizing player or not.
--Outputs the index of best move for player. bestVal will initially
--be (-inf).
minimax :: Board -> Bool -> Int
minimax board maximizing
                       | maximizing == True = undefined
                       | otherwise = undefined
                       where maxBest = infinity
                             minBest = - infinity

-- iterateBoard :: Board -> Field
-- iterateBoard [] = []
-- iterateBoard board = show 

maxRealFloat :: RealFloat a => a -> a
maxRealFloat x = encodeFloat b (e-1) `asTypeOf` x where
  b     = floatRadix x - 1
  (_,e) = floatRange x

infinity :: RealFloat a => a
infinity
       | isInfinite inf = inf
       | otherwise = maxRealFloat 1.0
       where inf = 1/0
---------------------------------------------------------------------------------------------------
