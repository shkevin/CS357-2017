{-
Name: Kevin Cox
NetID: Shkevin
-}

#!/usr/bin/env stack

module Homework2
    ( collatz, 
      haskellFileNames, 
      select, 
      prefixSum, 
      numbers, 
      Numeral, 
      makeLongInt, 
      evaluateLongInt, 
      changeRadixLongInt, 
      addLongInts, 
      mulLongInts
    ) where

--No other imports are allowed
import Data.List

-------------------------------------2.1-----------------------------------------
collatz :: [Int] -> Int
collatz [x] = x
collatz (x:xm:xs)
                | lenCollatzX > lenCollatzXm = collatz (x:xs)
                | lenCollatzX < lenCollatzXm = collatz (xm:xs)
                | otherwise = collatz ((maximum [x, xm]):xs)
                where lenCollatzX = length (indCollatz x)
                      lenCollatzXm = length (indCollatz xm)


--Works for increasing lits only
-- xs !! (last (getIndexOfVal (maximum $ map (length . indCollatz) xs) 0 (map (length . indCollatz) xs)))
---------------------------------------------------------------------------------

-------------------------------------2.2-----------------------------------------
haskellFileNames :: [String] -> [String]
haskellFileNames strings = filter (endsWith) strings 
---------------------------------------------------------------------------------

-------------------------------------2.3-----------------------------------------
select :: (t -> Bool) -> [t] -> [a] -> [a]
select p xs ys = [ys !! ind | ind <- elemIndices True $ map (p) xs]
---------------------------------------------------------------------------------

-------------------------------------2.4-----------------------------------------
prefixSum :: [Int] -> [Int]
prefixSum [] = []
prefixSum xs = (prefixSum (take (length xs + (-1)) xs)) ++ [(sum xs)]
---------------------------------------------------------------------------------

-------------------------------------2.5-----------------------------------------
numbers :: [Int] -> Int
numbers xs = read $ concatMap (show) xs :: Int
---------------------------------------------------------------------------------

-------------------------------------2.6-----------------------------------------
type Numeral = (Int, [Int])

example = (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0])

------------------------------------2.6 1----------------------------------------
makeLongInt :: Integer -> Int -> Numeral
makeLongInt n r = (r, changeBase (fromIntegral n) (toInteger r))
---------------------------------------------------------------------------------      

------------------------------------2.6 2----------------------------------------
evaluateLongInt :: Numeral -> Integer
evaluateLongInt (r, l) = toInteger $ sum $ map (\(x, y) -> (toInteger x * ((toInteger r) ^ y))) (zip l (generateDecList $ length l + (-1)))
---------------------------------------------------------------------------------

------------------------------------2.6 3----------------------------------------
changeRadixLongInt :: Numeral -> Int -> Numeral
changeRadixLongInt (or, l) nr = undefined
---------------------------------------------------------------------------------

------------------------------------2.6 4----------------------------------------
addLongInts :: Numeral -> Numeral -> Numeral
addLongInts (r1, l1) (r2, l2) = undefined 
---------------------------------------------------------------------------------

------------------------------------2.6 5----------------------------------------
mulLongInts :: Numeral -> Numeral -> Numeral
mulLongInts = undefined
---------------------------------------------------------------------------------


----------------------------User defined functions-------------------------------
indCollatz :: Int -> [Int]
indCollatz 1 = [1]
indCollatz x
           | (isEven x) == True = x:(indCollatz (x `div` 2))
           | otherwise = x:(indCollatz ((3*x) + 1))

isEven :: Int -> Bool
isEven x = ((x `mod` 2) == 0)

increment :: Int -> Int
increment x = x + 1

getIndexOfVal :: Int -> Int -> [Int] -> [Int]
getIndexOfVal _ _ [] = []
getIndexOfVal val count xs
                    | (take 1 xs) == [val] = [count] ++ getIndexOfVal val (increment count) (tail xs)
                    | otherwise = getIndexOfVal val (increment count) (tail xs)

endsWith :: String -> Bool
endsWith [] = False
endsWith str = substring ".hs" str || substring ".lhs" str

substring :: String -> String -> Bool
substring _ [] = False
substring str strToCheck
                       | str `isSuffixOf` strToCheck = True 
                       | head strToCheck == ' ' = substring str (tail strToCheck)
                       | (head . reverse) strToCheck == ' ' = substring str (take (length strToCheck + (-1)) strToCheck)
                       | otherwise = False

decomposeNum :: Integer -> [Int]
decomposeNum num = map (\x -> read [x]) $ show num

--Iteration needs to be 1 to have correct output
numLength :: Integer -> Int -> Int
numLength 0 _ = 0
numLength num iteration
                      | num `div` 10 == 0 = iteration
                      | otherwise = numLength (num `div` 10) (increment iteration)

-- test :: [Int] -> Int -> Integer
test n r = map (\(x, y) -> (fromIntegral) x * (r^y)) (zip (decomposeNum n) (generateDecList ((numLength n 0))))

changeBase :: Integer -> Integer -> [Int]
changeBase 0 _ = []
changeBase n r = changeBase (n `div` r) r ++ [fromInteger $ n `mod` r]

cb :: Int -> Int -> [Int]
cb 0 _ = []
cb n r = cb (n `div` r) r ++ [(n `mod` r)]

cbFromTo :: [Int] -> Int -> Int
cbFromTo [x] _ = x
cbFromTo (x:xm:xs) r = cbFromTo (((x * r) + xm):xs) r

fromListToInt :: [Int] -> Int
fromListToInt [] = 0
fromListToInt (x:xs) = (x * (10 ^ length xs)) + fromListToInt xs

intToList :: Integral x => x -> [x]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]


-- changeBase' :: [Int] -> Int -> [Int]
-- changeBase' l r = changeBase' ((head l) `div` r) r ++ [ `mod` r]

generateDecList :: Int -> [Int]
generateDecList 0 = [0]
generateDecList x = [fromIntegral x] ++ generateDecList (fromIntegral x +(-1))