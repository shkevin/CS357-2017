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
changeRadixLongInt (or, l) nr = if or == 10
                                then removeZeroPadding (fromBaseTenTo l nr [0])
                                else changeRadixLongInt (toBaseTen l or [0]) nr
---------------------------------------------------------------------------------

-- ------------------------------------2.6 4----------------------------------------
addLongInts :: Numeral -> Numeral -> Numeral
addLongInts (r1, l1) (r2, l2)
                            | r1 == r2 =  (r1, applyCarriesAdd (zipWith (+) (getLargerList l1 l2) (padFront (abs (length l1 - length l2)) (getSmallerList l2 l1) 0 )) r1)
                            | r1 < r2 = addLongInts (changeRadixLongInt (r1, l1) r2) (r2, l2)
                            | otherwise = addLongInts (r1, l1) (changeRadixLongInt (r2, l2) r1)

applyCarriesAdd :: [Int] -> Int -> [Int] 
applyCarriesAdd xs r = reverse (applyCarriesHelperAdd (reverse xs) r 0)

applyCarriesHelperAdd :: [Int] -> Int -> Int -> [Int]
applyCarriesHelperAdd [] _ 0 = []
applyCarriesHelperAdd [] _ carry = [carry]
applyCarriesHelperAdd (x:xs) r carry = [num `mod` r] ++ applyCarriesHelperAdd xs r (num `div` r)
                                       where num = x + carry
---------------------------------------------------------------------------------

------------------------------------2.6 5----------------------------------------
mulLongInts :: Numeral -> Numeral -> Numeral
mulLongInts(r1, l1) (r2, l2)
                           | r1 == r2 = (r1, convertList (applyCarriesAdd (sumLists (applyMults l1 l2 r1)) r1) r1)
                           | r1 < r2 = mulLongInts (changeRadixLongInt (r1, l1) r2) (r2, l2)
                           | otherwise = mulLongInts (r1, l1) (changeRadixLongInt (r2, l2) r1)

padLists :: [[Int]] -> Int -> [[Int]]
padSize [[]] _ = [[]]
padLists xs padSize = map (\x -> (padFront (padSize - length x) x 0)) xs

count :: [[a]] -> [Int]
count [] = []
count (x:xs) =  (length x):(count xs)

-- map (reverse) (map (`breakDownNum` 5) [3412,4,0,0])

convertList :: [Int] -> Int -> [Int]
convertList xs r =  concat (replace [] [0] list)
                    where list = map (reverse) (map (`breakDownNum` r) xs)
                          count = length $ filter (==[]) list

-- replace :: Eq b => [b] -> [b] -> [b] -> [b]
replace a b = map (\x -> if (a == x) then b else x)

combine :: [a] -> [a] -> [[a]]
combine xs ys = [xs,ys]

-- padFront (length (filter (==[]) xs)) (reverse $ concat list) 0
-- where list = map (reverse) (map (`breakDownNum` r) xs

breakDownNum :: Int -> Int -> [Int]
breakDownNum 0 _ = []
breakDownNum x r = [x `mod` r] ++ breakDownNum (x `div` r) r

sumLists :: [[Int]] -> [Int]
sumLists [[x]] = [x]
sumLists [[]] = []
sumLists (x:xs) = foldl (zipWith (+)) x xs 

applyMults :: [Int] -> [Int] -> Int -> [[Int]]
applyMults xs ys r = padLists op (maximum (count op))
                     where op = (map (reverse) (applyMultsHelper (reverse xs) (reverse ys)))

applyMultsHelper :: [Int]-> [Int]-> [[Int]]
applyMultsHelper [] _ = []
applyMultsHelper _ [] = []
applyMultsHelper xs (y:ys) = applyMultsHelper (0:xs) ys ++ [(multOneNumToList y xs)]

multOneNumToList :: Int -> [Int] -> [Int]
multOneNumToList x xs = map (x*) xs
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

changeBase :: Integer -> Integer -> [Int]
changeBase 0 _ = []
changeBase n r = changeBase (n `div` r) r ++ [fromInteger $ n `mod` r]

--Converts from base 10 to base r
cb :: Int -> Int -> [Int]
cb 0 _ = []
cb n r = cb (n `div` r) r ++ [(n `mod` r)]

--Converts to base 10 from given radix r
convertToTen :: Int -> Int -> Int
convertToTen n r = sum $ map (\(x,y) ->  x* (r ^ y)) (zip (intToList n) (generateDecList $ ((length $ intToList n)) + (-1)))

toBaseTen :: [Int] -> Int -> [Int] -> Numeral
toBaseTen [] r i = (10, i)
toBaseTen (x:xs) r i = toBaseTen xs r op
                   where 
                      m = mulLongInts (10, i) (10, [r])
                      a = addLongInts m (10, [x])
                      op = snd a

cbFromTo :: [Int] -> Int -> Int -> Int
cbFromTo [x] _ _ = x
cbFromTo (x:xm:xs) or nr = cbFromTo (result : xs) or nr
                           where result = cbFromTo [x * or + xm] or nr

fromBaseTenTo :: [Int] -> Int -> [Int] -> Numeral
fromBaseTenTo [] r i = (r, i)
fromBaseTenTo (x:xs) r i = fromBaseTenTo xs r op
                           where m = mulLongInts (r, i) (r, mult)
                                 a = addLongInts m (r, [x])
                                 op = snd a
                                 mult = intToListFrom r 10

fromListToInt :: [Int] -> Int
fromListToInt [] = 0
fromListToInt (x:xs) = (x * (10 ^ length xs)) + fromListToInt xs

--This is just the base 10 version
intToList :: Int -> [Int]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]

intToListFrom :: Int -> Int -> [Int]
intToListFrom r 0 = []
intToListFrom r x = intToListFrom r (x `div` r) ++ [x `mod` r]

generateDecList :: Int -> [Int]
generateDecList 0 = [0]
generateDecList x = [fromIntegral x] ++ generateDecList (fromIntegral x +(-1))

padFront :: Int -> [Int] -> Int -> [Int]
padFront len xs padding = (take len (repeat padding)) ++ xs

removeZeroPadding :: Numeral -> Numeral
removeZeroPadding (b, (x:xs))
                            | x == 0 = removeZeroPadding (b, xs)
                            | otherwise = (b, (x:xs))

getLargerList :: [Int] -> [Int] -> [Int]
getLargerList xs ys
                  | length xs >= length ys = xs
                  | otherwise = ys

getSmallerList :: [Int] -> [Int] -> [Int]
getSmallerList xs ys
                    | length xs <= length ys = xs
                    | otherwise = ys

getCarry :: Int -> Int -> Int
getCarry x r
           | x `div` r == 0 = (x `mod` r)
           | otherwise = getCarry (x `div` r) r

func a [] = [a]
func a (x:xs) = x : func a xs