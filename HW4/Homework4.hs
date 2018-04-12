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
insertions [] = [['A']] ++ [['T']] ++ [['G']] ++ [['C']]
insertions (x:xs) = [['A'] ++ (x:xs)] ++ [['T'] ++ (x:xs)] ++ 
    [['G'] ++ (x:xs)] ++ [['C'] ++ (x:xs)] ++ (map (x:) (insertions xs))

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
deletions :: String -> [String]
deletions "" = [""]
deletions (x:"") = [""]
deletions (x:xs) = [xs] ++ (map (x:) (deletions xs))

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
substitutions :: String -> [String]
substitutions "" = [""]
substitutions (x:xs) = take ((length $ subs (x:xs)) - 1) (subs (x:xs))

subs :: String -> [String]
subs "" = [""]
subs (x:xs) = [['A']++ xs]++[['T']++ xs] ++
    [['G']++ xs] ++ [['C']++ xs] ++ (map (x:) (subs xs))

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
transpositions :: String -> [String]
transpositions "" = [[]]
transpositions (a:"") = [[a]]
transpositions (x:y:[]) = [(y:x:[])]
transpositions (x:y:xs) = [(y:x:xs)] ++ map (x:) (transpositions (y:xs))

-------------------------------4.2 Sorting (20pts)------------------------------
{-
  * PARAMETERS: Element to add, sorted List to add element to.
  * FUNCTION: Inserts an element into the corect position in a sort list.
  * RETURNS: Sorted list with added element.
-}
insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert x (y:ys)
              | y <= x = x:y:ys
              | otherwise = y:insert x ys

{-
  * PARAMETERS: List to be sorted.
  * FUNCTION: Sorts a list into the correct order using insertion sort.
  * RETURNS: List of sorted elements.
-}
isort :: Ord a => [a] -> [a]
isort [] = []
isort xs = reverse $ (isortHelper xs)
           where 
                isortHelper :: Ord a => [a] -> [a]
                isortHelper [] = []
                isortHelper (x:xs) = insert x (isortHelper xs)

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
--can use readFile, getContents, words. 
fileisort :: String -> String -> IO ()
fileisort fn1 fn2 = writeFile fn2 . unlines . isort . lines =<< readFile fn1
-----------------------------4.3 Game Trees (40pts)-----------------------------
data Field = B | R | G
             deriving (Eq, Ord, Show)
type Board = [Field]

data Tree a = Node a [Tree a] 
              deriving Show

size :: Int
size = 3

depth :: Int
depth = 9

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
strategyForRed :: Board -> Int
strategyForRed board = genericStrat board R

{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
strategyForGreen :: Board -> Int
strategyForGreen board = genericStrat board G

genericStrat :: Board -> Field -> Int
genericStrat board field =  head $ L.elemIndices (True) listOfBools
                            where listOfBools = help $ zip (concat $ bestmove (splitEvery 3 board) field) board

help :: [(Field, Field)] -> [Bool]
help [] = [False]
help (x:xs)
                    | fst x == snd x = False : help xs
                    | otherwise = True : help xs

next :: Field -> Field
next G = R
next R = G
next B = B

getWinner :: [Board] -> Field
getWinner board = if wins G board
                  then G
                  else R

empty :: [Board]
empty = replicate size (replicate size B)

full :: [Board] -> Bool
full = all (/= B) . concat

turn :: [Board] -> Field
turn board = if gs <= rs then G else R
         where
            rs = length (filter (==R) ps)
            gs = length (filter (==G) ps)
            ps = concat board

wins :: Field -> [Board] -> Bool
wins field board = any line (rows ++ cols ++ dias)
                   where
                      line = all (== field)
                      rows = board
                      cols = L.transpose board
                      dias = [diag board, diag (map reverse board)]

diag :: [Board] -> Board
diag board = [board !! n !! n | n <- [0..size-1]]

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
                    where
                       (first, rest) = splitAt n list

won :: [Board] -> Bool
won board = wins G board || wins R board

putBoard :: [Board] -> IO ()
putBoard =
          putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size*4)-1) '-']

showRow :: Board -> [String]
showRow = beside . interleave bar . map showField
          where
             beside = foldr1 (zipWith (++))
             bar = replicate 3 "|"

showField :: Field -> [String]
showField R = ["   ", " R ", "   "]
showField B = ["   ", "   ", "   "]
showField G = ["   ", " G ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: [Board] -> Int -> Bool
valid board i = 0 <= i && i < size^2 && concat board !! i == B

move :: [Board] -> Int -> Field -> [[Board]]
move board i field = 
                   if valid board i then [chop size (xs ++ [field] ++ ys)] else []
                   where (xs, B:ys) = splitAt i (concat board)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

gametree :: [Board] -> Field -> Tree [Board]
gametree board field = Node board [gametree g' (next field) | g' <- moves board field]

moves :: [Board] -> Field -> [[Board]]
moves board field
                 | won board = []
                 | full board = []
                 | otherwise = concat [move board i field | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

minimax :: Tree [Board] -> Tree ([Board], Field) 
minimax (Node board [])                                                     --G == 0, R = X
                      | wins G board = Node (board, G) []
                      | wins R board = Node (board, R) []
                      | otherwise = Node (board, B) []
minimax (Node board ts)
                      | turn board == G = Node (board, minimum ps) ts'
                      | turn board == R = Node (board, maximum ps) ts'
                                          where
                                             ts' = map minimax ts
                                             ps = [p | Node (_,p) _ <- ts']

bestmove :: [Board] -> Field -> [Board]
bestmove board field = head [g' | Node (g',field') _ <- ts, field' == best]
                       where
                          tree = prune depth (gametree board field)
                          Node (_,best) ts = minimax tree

------------4.4 (Optional) Drawing Game Trees and Strategies (30pts EC)---------
{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
drawStrategy :: Bool -> String -> IO ()
drawStrategy = undefined
