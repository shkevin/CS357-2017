{-
Name: Kevin Cox
NetID: Shkevin
-}

module Homework4 where

--No other imports allowed
import qualified Data.List as L
import System.Random
import System.IO.Unsafe

-------------------------------4.1 Genome Lists (40pts)--------------------------
{-
  * PARAMETERS: Bases to insert.
  * FUNCTION: A base is inserted between two adjacent points in a genome.
  * RETURNS: List of all genomes where the given bases have been inserted.
-}
insertions :: String -> [String]
insertions [] = [['A']] ++ [['T']] ++ [['G']] ++ [['C']]
insertions (x:xs) = [['A'] ++ (x:xs)] ++ [['T'] ++ (x:xs)] ++ 
    [['G'] ++ (x:xs)] ++ [['C'] ++ (x:xs)] ++ (map (x:) (insertions xs))

{-
  * PARAMETERS: Bases to delete.
  * FUNCTION: A point is deleted from a genome.
  * RETURNS: list of all genomes that exclude the given bases to delete. 
-}
deletions :: String -> [String]
deletions "" = [""]
deletions (x:"") = [""]
deletions (x:xs) = [xs] ++ (map (x:) (deletions xs))

{-
  * PARAMETERS: Bases to substitute
  * FUNCTION: A base at a point is replaced with another base.
  * RETURNS: List of all genomes with the given substitution applied.
-}
substitutions :: String -> [String]
substitutions xs = aS ++ gS ++ cS ++ tS
                where
                   aS = applyToAll xs 'A' 0
                   gS = applyToAll xs 'G' 0
                   cS = applyToAll xs 'C' 0
                   tS = applyToAll xs 'T' 0

charAtIndex :: String -> Char -> Int -> String
charAtIndex xs char i = front ++ [char] ++ back
                        where
                           front = take i xs
                           back = drop (i+1) xs

applyToAll :: String -> Char -> Int -> [String]
applyToAll xs char n
                 | (length xs) == n = []
                 | otherwise = charAtIndex xs char n : applyToAll xs char (n+1)

{-
  * PARAMETERS: Bases to transpose.
  * FUNCTION: The bases at two adjacent points are exchanged.
  * RETURNS: List of all genomes with the given tranpose applied.
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
  * PARAMETERS: Filename 1, Filename 2.
  * FUNCTION: Sorts the given file and outputs to file 2.
  * RETURNS: Sorted filename.
-}
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

seed :: Int
seed = unsafePerformIO (getStdRandom (randomR (0, maxBound::Int)))

{-
 The idea behind both strategies is using minimax, but maximizing the field when needed, and
 minimizing the field when needed. This has been tested with random games, as well as game boards
 designed to try and break it. It will always end in a draw or with red winning. You can run some of the test code by
 calling -playRandom n-. This will play n games where G is played Randomly. This proves that the algorithm is perfect 
 for both players since both of them are using the same algorithm. You can show that they always draw by calling
 -play empty R-. This plays R against G, where the algorithms are the same. This always results in a draw.
-}


{-
  * PARAMETERS: Board for Red to play.
  * FUNCTION: Assess the given board for best move for R. 
  * RETURNS: Int value for the best move for R.
-}
strategyForRed :: Board -> Int
strategyForRed board = head $ L.elemIndices (True) listOfBools
                            where listOfBools = help $ zip (concat $ bestmove (splitEvery 3 board) R) board

{-
  * PARAMETERS: Board for Green to play.
  * FUNCTION: Assess the given board for best move for G. 
  * RETURNS: Int value for the best move for G.
-}
strategyForGreen :: Board -> Int
strategyForGreen board = head $ L.elemIndices (True) listOfBools
                            where listOfBools = help $ zip (concat $ bestmoveForGreen (splitEvery 3 board) G) board


genericStrat :: Board -> Field -> Int
genericStrat board field =  head $ L.elemIndices (True) listOfBools
                            where listOfBools = help $ zip (concat $ bestmove (splitEvery 3 board) field) board

help :: [(Field, Field)] -> [Bool]
help [] = [False]
help (x:xs)
          | fst x == snd x = False : help xs
          | otherwise = True : help xs

playPrint :: [Board] -> Field -> IO ()
playPrint board field = do
                        putBoard board
                        play board field

play :: [Board] -> Field -> IO ()
play board field
               | wins G board = putStrLn "Player G wins!\n"
               | wins R board = putStrLn "Player R wins!\n"
               | full board = putStrLn "Draw!\n"
               | otherwise = (playPrint (bestmove board field)) (nextField field)

playString :: [Board] -> Field -> String
playString board field
                      | wins G board = "Player G wins!\n"
                      | wins R board = "Player R wins!\n"
                      | full board = "Draw!\n"
                      | otherwise = playString (bestmove board field) (nextField field)

playNineGames :: [Board] -> [String]
playNineGames [] = ["Done"]
playNineGames (x:xs) = (playString (splitEvery 3 x) G) : playNineGames xs

-- playRandom :: [Board] -> Field -> [String]
-- playRandom board field = 

generateBoards :: [Board]
generateBoards = splitEvery 9 [boards | n <- [0..8], boards <- (replaceNth n R (concat empty))]

generateRandomNumbers :: Int -> [Int]
generateRandomNumbers n = take n . randomRs (0, 8) . mkStdGen $ seed

firstValidMove :: [Board] -> Int
firstValidMove board = if length x /= 0
                       then head $ x
                       else -1
                       where x = [x | x <- generateRandomNumbers 9, valid board x]

playRandom :: Int -> [String]
playRandom 0 = ["Done"]
playRandom n = playOneRandom empty R : playRandom (n-1)

playOneRandom :: [Board] -> Field -> String
playOneRandom board field 
                        | wins G board = "Player G wins!\n"
                        | wins R board = "Player R wins!\n" 
                        | full board = "Draw!\n"
                        | validMove == -1 = "Error"
                        | field == R = playOneRandom (bestmove board field) (nextField field)
                        | field == G = playOneRandom (splitEvery 3 (replaceNth (validMove) field (concat board))) (nextField field)
                        where validMove = firstValidMove board
                
replaceNth n newVal (x:xs)
                         | n == 0 = newVal:xs
                         | otherwise = x:replaceNth (n-1) newVal xs

nextField :: Field -> Field
nextField G = R
nextField R = G
nextField B = B

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
gametree board field = Node board [gametree g' (nextField field) | g' <- moves board field]

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

minimaxForGreen :: Tree [Board] -> Tree ([Board], Field) 
minimaxForGreen (Node board [])                                                     --G == 0, R = X
                      | wins G board = Node (board, G) []
                      | wins R board = Node (board, R) []
                      | otherwise = Node (board, B) []
minimaxForGreen (Node board ts)
                      | turn board == G = Node (board, maximum ps) ts'
                      | turn board == R = Node (board, minimum ps) ts'
                                          where
                                             ts' = map minimaxForGreen ts
                                             ps = [p | Node (_,p) _ <- ts']                                             

bestmove :: [Board] -> Field -> [Board]
bestmove board field = head [g' | Node (g',field') _ <- ts, field' == best]
                       where
                          tree = prune depth (gametree board field)
                          Node (_,best) ts = minimax tree

bestmoveForGreen :: [Board] -> Field -> [Board]
bestmoveForGreen board field = head [g' | Node (g',field') _ <- ts, field' == best]
                       where
                          tree = prune depth (gametree board field)
                          Node (_,best) ts = minimaxForGreen tree

------------4.4 (Optional) Drawing Game Trees and Strategies (30pts EC)---------
{-
  * PARAMETERS:
  * FUNCTION:
  * RETURNS:
-}
drawStrategy :: Bool -> String -> IO ()
drawStrategy = undefined
