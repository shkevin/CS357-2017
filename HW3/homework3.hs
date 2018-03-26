{-
Name: Kevin Cox
NetID: Shkevin
-}

#!/usr/bin/env stack

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
goldbach n = [(x,y) | x <- sieve n, y <- sieve n, x + y == n]

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
--map (x:) (powerset xs) ++ powerset xs
---------------------------------------------------------------------------------------------------

----------------------------------------------3.5--------------------------------------------------
example :: [[(Double, Double)]]
example = [[(100.0,100.0),(100.0,200.0),(200.0,100.0)],
  [(150.0,150.0),(150.0,200.0),(200.0,200.0),(200.0,150.0)]]

header = "%!PS-Adobe-3.0 EPSF-3.0\n%%BoundingBox: "
footer = "showpage\n%%EOF\n"

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
  * PARAMETERS: 
  * FUNCTION: 
  * RETURNS: 
-}
allpaths :: T -> [P]
allpaths = undefined
---------------------------------------------------------------------------------------------------

----------------------------------------------3.7--------------------------------------------------
type Expr = [[Int]]

-- [[-1, 2, 4], [-2, -3]] == (¬x1 ∨ x2 ∨ x4) ∧ (¬x2 ∨ ¬x3)
{-
  * PARAMETERS: 
  * FUNCTION: 
  * RETURNS: 
-}
eval :: (Int -> Bool) -> Expr -> Bool
eval = undefined

{-
  * PARAMETERS: 
  * FUNCTION: 
  * RETURNS: 
-}
satisfiable :: Expr -> Bool
satisfiable = undefined
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
