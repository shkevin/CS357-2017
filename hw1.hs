#!/usr/bin/env stack

main :: IO ()
main = putStrLn "Hello World"

test :: Int -> Int -> Bool
test a b = ((a `mod` 2) == 1) && ((b `mod` 2) == 1)

stutter :: [Char] -> [Char]
stutter [] = []
stutter (x:xs) = [x] ++ [x] ++ (stutter xs)

compress :: [Char] -> [Char]
compress [] = []
compress (x:xs) = if [x] == take 1 xs
                  then compress xs
                  else x:compress xs

zipSum :: [Int] -> [Int] -> [Int]
zipSum [] _ = []
zipSum (x:xs) (y:ys) = [x + y] ++ zipSum xs ys

removeElement :: Eq a => a -> [a] -> [a]
removeElement _ [] = []
removeElement x (y:ys) = if x == y
                         then removeElement x ys
                         else y:removeElement x ys

setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion [] list = list
setUnion (x:xs) list = if elem x list
                       then x:setUnion xs (removeElement x list)
                       else x:setUnion xs list

setIntersection :: [Integer] -> [Integer] -> [Integer]
setIntersection [] _ = []
setIntersection (x:xs) list = if elem x list
                                then x:setIntersection xs (removeElement x list)
                                else setIntersection xs list

setDifference :: [Integer] -> [Integer] -> [Integer]
setDifference [] _ = []

setEqual :: [Integer] -> [Integer] -> Bool
setEqual [] _ = 
setEqual (x:xs) (x:ys) = if x == y
                         then true && setEqual xs ys
                         else false