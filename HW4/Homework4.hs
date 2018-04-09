#!/usr/bin/env stack

module Homework4 where

--No other imports allowed
import qualified Data.List as L

-------------------------------4.1 Genome Lists (40pts)--------------------------
{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
insertions :: String -> [String]
insertions = undefined

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
deletions :: String -> [String]
deletions = undefined

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
substitutions :: String -> [String]
substitutions = undefined

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
transpositions :: String -> [String]
transpositions = undefined

-------------------------------4.2 Sorting (20pts)------------------------------
{-
  * PARAMETERS: Element to add, sorted List to add element to.
  * FUNCTION: Inserts an element into the corect position in a sort list.
  * RETURNS: Sorted list with added element.
-}
insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x:xs)
              | y <= x = y:x:xs ++ insert y xs
              | otherwise = x : insert y xs

{-
  * PARAMETERS: List to be sorted.
  * FUNCTION: Sorts a list into the correct order using insertion sort.
  * RETURNS: List of sorted elements.
-}
isort :: Ord a => [a] -> [a]
isort [] = []
isort xs = take (length xs) (insert (head xs) (isort (tail xs)))

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
--can use readFile, getContents, words. 
fileisort :: String -> String -> IO ()
fileisort = undefined

test = do x <- readFile "D:/ProgramFiles/Shkevin/cs357/HW4/text.txt"
          putStr x
-----------------------------4.3 Game Trees (40pts)-----------------------------
data Field = B | R | G
             deriving (Eq, Ord, Show)
type Board = [Field]

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
strategyForRed :: Board -> Int
strategyForRed = undefined

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
strategyForGreen :: Board -> Int
strategyForGreen = undefined

------------4.4 (Optional) Drawing Game Trees and Strategies (30pts EC)---------
{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
drawStrategy :: Bool -> String -> IO ()
drawStrategy = undefined
