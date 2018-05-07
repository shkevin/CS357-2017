\begin{code}
import Data.List
import Data.Ord
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

------------------------------5.1 Trees (40 pts)------------------------------
data Tree a = E
            | T a (Tree a) (Tree a)
            deriving (Eq, Show)

{-
  * PARAMETERS: Given tree to reconstruct.
  * FUNCTION: Creates a tree of the same shape as the input tree, and replaces
  			  the values of the nodes with 1..N.
  * RETURNS: Tree with parent nodes of Ints.
-}
bfnum :: Tree a -> Tree Int
bfnum t = head (bfnum' 1 [t])

{-
	The idea of the following code: My initial idea of how to solve this was by somehow flattening
	the tree and rebuilding it with replaced parent nodes. This however didn't lead me anywhere (old code below),
    so then I tried to re-number the parents based off of levels instead. So rather than flattening
    the entire tree, then rebuilding, it re-numbers one level at a time and tries to rebuild the tree
    from those levels. I think the algorithm would run in O(nlogn) or O(n^2). Didn't really have a lot
    of time to delve too far into the efficiency (finals blow).
-}

{-
  * PARAMETERS: Given tree of children to get.
  * FUNCTION: Returns a list of children of the given level in the Tree.
  * RETURNS: List of Tree with children nodes.
-}
getChildren :: Tree a -> [Tree a]
getChildren E = []
getChildren (T a l r) = [l, r]

{-
  * PARAMETERS: Depth of level, Current level, Next Level.
  * FUNCTION: Combines the trees together after replaces the parent nodes with bf numbering.
  * RETURNS: List of Tree of Ints.
-}
rebuildTree :: Int -> [Tree a] -> [Tree Int] -> [Tree Int]
rebuildTree depth [] [] = []
rebuildTree depth (E : xs) ys = E : rebuildTree depth xs ys
rebuildTree depth ((T _ _ _) : xs) (l : r : ys) = (T depth l r) : rebuildTree (depth + 1) xs ys

{-
  * PARAMETERS: Depth of level (starts at 1), List of Tree to re-number.
  * FUNCTION: Re-numbers the given Tree with the breadth-first number.
  * RETURNS: List of re-numbered Tree of Ints.
-}
bfnum' :: Int -> [Tree a] -> [Tree Int]
bfnum' depth [] = []
bfnum' depth level = rebuildTree depth level nextLevel'
                     where 
                        nextLevel = concat (map getChildren level)
                        numNodes = depth + (length nextLevel `div` 2)
                        nextLevel' = bfnum' numNodes nextLevel

getNumbersForTree :: Tree a -> [Int]
getNumbersForTree tree = [1..length $ bfs tree]

-- This produces 1 more leaf than needed
buildFlattenTree :: Tree a -> Int -> [(Int, [a])]
buildFlattenTree E n = [(n, [])]
buildFlattenTree (T a lt rt) n = (n, [a]) : (buildFlattenTree lt (leftTreeVal n)) ++ (buildFlattenTree rt (rightTreeVal n))
                   
unFlattenTree' :: [(Int, [a])] -> Tree Int
unFlattenTree' [] = E
unFlattenTree' ((n, val):xs) = T n (unFlattenTree' lt) (unFlattenTree' rt)
                                 where
                                    (lt, rt) = splitAt (length xs `div` 2) xs

orderTree :: [(Int, [a])] -> [(Int, [a])]
orderTree list = sortBy (comparing fst) list

lengthOfTree :: Tree a -> Int
lengthOfTree E = 1
lengthOfTree (T a lt rt) = (lengthOfTree lt) + (lengthOfTree rt) + 1

rightTreeVal :: Int -> Int
rightTreeVal n = (n * 2) + 2

leftTreeVal :: Int -> Int
leftTreeVal n = (n * 2) + 1

--Called with dfs (T 'a' (T 'b' E (T 'c' E E)) (T 'd' E E)). O(V + E)
dfs :: Tree a -> [a]
dfs E = []
dfs (T a lt rt) = a : (dfs lt) ++ (dfs rt)

--Called with bfs (T 'a' (T 'b' E (T 'c' E E)) (T 'd' E E)). O(V + E)
bfs :: Tree a -> [a]
bfs tree = bf [tree]
           where
              bf [] = []
              bf xs = map val xs ++ bf (concat (map lnR xs))
              val (T a _ _ ) = a
              lnR (T _ E E) = []
              lnR (T _ E b) = [b]
              lnR (T _ a E) = [a]
              lnR (T _ a b) = [a,b]

-------------------------5.2 Expression Trees (30 pts)------------------------
type Identifier = String

data Expr = Num Integer
          | Var Identifier
          | Let {var :: Identifier, value :: Expr, body :: Expr}
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Eq)

type Env = Identifier -> Integer

{-
  * PARAMETERS: 
  * FUNCTION: 
  * RETURNS: 
-}
emptyEnv :: Env
emptyEnv = \s -> error ("unbound: " ++ s)

{-
  * PARAMETERS: 
  * FUNCTION: 
  * RETURNS: 
-}
extendEnv :: Env -> Identifier -> Integer -> Env
extendEnv oldEnv s n s' = if s' == s then n else oldEnv s'

{-
  * PARAMETERS: 
  * FUNCTION: 
  * RETURNS: 
-}
evalInEnv :: Env -> Expr -> Integer
evalInEnv = undefined

-------------------------5.3 Infinite Lists (30 pts)--------------------------
{-
  * PARAMETERS: 
  * FUNCTION: 
  * RETURNS: 
-}
diag :: [[a]] -> [a]
diag = undefined


\end{code}