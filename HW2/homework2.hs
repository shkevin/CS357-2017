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
changeRadixLongInt (or, l) nr
                             | nr == or = (or, l)
                             | otherwise = (nr, radixChange (minimum [or, nr]) (maximum [or, nr]) l)

radixChange :: Int -> Int -> [Int] -> [Int]
radixChange _ _ [] = []
radixChange or nr (x:y:xs) = [(x * or) + (y)]
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
-- mulLongInts :: Numeral -> Numeral -> Numeral
-- mulLongInts(r1, l1) (r2, l2)
--                            | r1 == r2 = (r1, applyCarries (applyMult (getLargerList l1 l2) (getSmallerList l2 l1) r1) r1)
--                            | r1 < r2 = mulLongInts (changeRadixLongInt (r1, l1) r2) (r2, l2)
--                            | otherwise = mulLongInts (changeRadixLongInt (r1, l1) r2) (r2, l2)

-- applyMult :: [Int] -> [Int] -> Int -> [Int]
-- applyMult xs ys r
--                 | length op > 1 =  map fromListToInt $ map (`cb` r) (sumLists op)
--                 | otherwise = map fromListToInt $ map (`cb` r) (head op)
--                 where op = (applyMultHelper (reverse $ map (`convertToTen` r) xs) (reverse $ map (`convertToTen` r) ys))

-- applyMultHelper :: [Int] -> [Int] -> [[Int]]
-- applyMultHelper _ [] = []
-- applyMultHelper xs (y:ys) = (applyMultHelper (0:xs) ys) ++ [reverse (map (y*) xs)]

-- sumLists :: [[Int]] -> [Int]
-- sumLists [[]] = []
-- sumLists (x:y:xs) = zipWith (+) (getLargerList x y) (padFront (abs (length x - length y)) (getSmallerList y x) 0)




mulLongInts :: Numeral -> Numeral -> Numeral
mulLongInts(r1, l1) (r2, l2)
                           | r1 == r2 = (r1, applyCarriesAdd (sumLists (applyMults l1 l2 r1)) r1)
                           | r1 < r2 = mulLongInts (changeRadixLongInt (r1, l1) r2) (r2, l2)
                           | otherwise = mulLongInts (r1, l1) (changeRadixLongInt (r2, l2) r1)

-- applyMult :: [Int] -> [Int] -> [Int]
-- applyMult xs ys
--                | length op > 1 = sumLists (padLists op (maximum (count op)))
--                | otherwise = head op
--                where op = (applyMultHelper (reverse xs) (reverse ys))

-- applyMultHelper :: [Int] -> [Int] -> [[Int]]
-- applyMultHelper _ [] = []
-- applyMultHelper xs (y:ys) = (applyMultHelper (0:xs) ys) ++ [reverse (map (y*) xs)]

padLists :: [[Int]] -> Int -> [[Int]]
padSize [[]] _ = [[]]
padLists xs padSize = map (\x -> (padFront (padSize - length x) x 0)) xs

count :: [[a]] -> [Int]
count [] = []
count (x:xs) =  (length x):(count xs)

-- sumLists :: [[Int]] -> [Int]
-- sumLists [[x]] = [x]
-- sumLists [[]] = []
-- sumLists (x:y:xs) = (zipWith (+) (getLargerList x y) (padFront (abs (length x - length y)) (getSmallerList y x) 0))

sumLists :: [[Int]] -> [Int]
sumLists [[x]] = [x]
sumLists [[]] = []
sumLists (x:xs) = foldl (zipWith (+)) x xs 

applyMults :: [Int] -> [Int] -> Int -> [[Int]]
applyMults xs ys r = padLists op (maximum (count op))
                     where op = (map (reverse) (applyMultsHelper (reverse xs) (reverse ys)))
--this one sort of works
-- applyCarriesHelperMult :: [Int] -> Int -> [Int]
-- applyCarriesHelperMult [x] r =  (breakDownNum x r) ++ [(getCarry x r)]
-- applyCarriesHelperMult (x:y:xs) r = (breakDownNum x r) ++ applyCarriesHelperMult ((y + (getCarry x r)):xs) r

-- applyCarriesHelperMult :: [Int] -> Int -> [Int]
-- applyCarriesHelperMult [x] r =  (breakDownNum x r) ++ [(getCarry x r)]
-- applyCarriesHelperMult (x:y:xs) r
--                                 | x `div` r /= 0 = (breakDownNum x r) ++ applyCarriesHelperMult ((y + (getCarry x r):xs)) r
--                                 | otherwise = [(getCarry x r)] ++ applyCarriesHelperMult (y:xs) r


applyMultsHelper :: [Int]-> [Int]-> [[Int]]
applyMultsHelper [] _ = []
applyMultsHelper _ [] = []
applyMultsHelper xs (y:ys) = applyMultsHelper (0:xs) ys ++ [(multOneNumToList y xs)]

multOneNumToList :: Int -> [Int] -> [Int]
multOneNumToList x xs = map (x*) xs

getCarry :: Int -> Int -> Int
getCarry x r
           | x `div` r == 0 = (x `mod` r)
           | otherwise = getCarry (x `div` r) r

func a [] = [a]
func a (x:xs) = x : func a xs
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

cbFromTo :: [Int] -> Int -> Int -> Int
cbFromTo [x] _ _ = x
cbFromTo (x:xm:xs) or nr = cbFromTo (result : xs) or nr
                           where result = cbFromTo [x * or + xm] or nr

fromListToInt :: [Int] -> Int
fromListToInt [] = 0
fromListToInt (x:xs) = (x * (10 ^ length xs)) + fromListToInt xs

breakDownNum :: Int -> Int -> [Int]
breakDownNum 0 _ = []
breakDownNum x r
               | x `div` r /= 0 = [x `mod` r] ++ breakDownNum (x `div` r ) r
               | otherwise = []

intToList :: Int -> [Int]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]

generateDecList :: Int -> [Int]
generateDecList 0 = [0]
generateDecList x = [fromIntegral x] ++ generateDecList (fromIntegral x +(-1))

padFront :: Int -> [Int] -> Int -> [Int]
padFront len xs padding = (take len (repeat padding)) ++ xs

getLargerList :: [Int] -> [Int] -> [Int]
getLargerList xs ys
                  | length xs >= length ys = xs
                  | otherwise = ys

getSmallerList :: [Int] -> [Int] -> [Int]
getSmallerList xs ys
                    | length xs <= length ys = xs
                    | otherwise = ys