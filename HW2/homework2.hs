{-
YourName
YourNetId
-}

--No other imports are allowed
import Data.List

--2.1
collatz :: [Int] -> Int
collatz [] = []
collatz (x:xs)
             | length largest < length $indCollatz $take 1 xs = 
             | length largest > length $indCollatz $take 1 xs = 
             | otherwise = 

indCollatz :: Int -> [Int]
indCollatz 1 = [1]
indCollatz x
           | (isEven x) == True = [x] ++ (indCollatz (x `div` 2))
           | otherwise = [x] ++ (indCollatz ((3*x) + 1))


--2.2
haskellFileNames :: [String] -> [String]
haskellFileNames = undefined

--2.3
select :: (t -> Bool) -> [t] -> [a] -> [a]
select = undefined

--2.4
prefixSum :: [Int] -> [Int]
prefixSum = undefined

--2.5
numbers :: [Int] -> Int
numbers = undefined

--2.6
type Numeral = (Int, [Int])

example = (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0])

--2.6 1
makeLongInt :: Integer -> Int -> Numeral
makeLongInt = undefined

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
isEven :: Int -> Bool
isEven x = ((x `mod` 2) == 0)