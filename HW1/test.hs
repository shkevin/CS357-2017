#!/usr/bin/env stack

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p [] = []
-- filter p (x:xs) 
-- 	| p x = x:filter p xs
-- 	| otherwise = filter p xs

-- concat :: [[a]] -> [a]
-- concat [] = []
-- concat (b:bs) = b++concat bs

-- concat with list comprehension
-- concat' :: [[a]] -> [a]
-- concat' xss = [x | xs <- xss, x <- xs]

-- filter in list comprehension
-- filer p xs = [x | x <- xs, p x]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- isSorted :: [Int] -> Bool
-- isSorted (x:xs)
--     | x <= (take 1 xs) = True && isSorted xs
--     | otherwise = False

positions :: Int -> [Int] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x'==x]