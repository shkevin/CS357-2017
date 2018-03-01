{-
Name: Kevin Cox
NetID: Shkevin
-}

#!/usr/bin/env stack

--No other imports are allowed
import Data.List

--2.1
collatz :: [Int] -> Int
collatz xs = xs !! (last (getIndexOfVal (maximum $ map (length . indCollatz) xs) 0 (map (length. indCollatz) xs)))

--2.2
haskellFileNames :: [String] -> [String]
haskellFileNames strings = filter (endsWith) strings 

--2.3
select :: (t -> Bool) -> [t] -> [a] -> [a]
select p xs ys = [ys !! ind | ind <- elemIndices True $ map (p) xs]

--2.4
prefixSum :: [Int] -> [Int]
prefixSum [] = []
prefixSum xs = (prefixSum (take (length xs + (-1)) xs)) ++ [(sum xs)]

--2.5
numbers :: [Int] -> Int
numbers xs = sum $ zipWith (*) xs (reverse $ take (length xs) (iterate(10*)1))

--2.6
type Numeral = (Int, [Int])

example = (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0])

--2.6 1
-- makeLongInt :: Integer -> Int -> Numeral
-- makeLongInt n r = (r, [x | x <- (fromInteger n)])

--2.6 2
evaluateLongInt :: Numeral -> Integer
evaluateLongInt = undefined

--2.6 3
changeRadixLongInt :: Numeral -> Int -> Numeral 
changeRadixLongInt = undefined

--2.6 4
addLongInts :: Numeral -> Numeral -> Numeral
addLongInts = undefined

--2.6 5
mulLongInts :: Numeral -> Numeral -> Numeral
mulLongInts = undefined




--User defined functions
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

-- decomposeNum :: Integer -> [Int]
-- decomposeNum 0 = 0
-- decomposeNum num = 

-- length of  num = (length $ intersperse ',' "123") + (-2)