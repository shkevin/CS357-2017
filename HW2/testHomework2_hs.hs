{-
File for testing solutions to Homework 2. 

Add the following to the top of your homework2.hs file:

module Homework2 
( collatz  
, haskellFileNames 
, select  
, prefixSum  
, numbers  
, makeLongInt
, evaluateLongInt
, changeRadixLongInt
, addLongInts
, mulLongInts
) where

-}

import Data.List
import Homework2

-- All of homework 2
testAll = and [
    testCollatz, 
    testHaskellFileNames, 
    testSelect, 
    testPrefixSum, 
    testNumbers, 
    testNumeral
  ]

-- 2.1
testCollatz = and [
    collatz [1..20] == 19,
    collatz [20,19..1] == 19,
    collatz [1..1000] == 871,
    collatz [1000,999..1] == 871,
    collatz [9223372036854775805,2] == 9223372036854775805
  ]

-- 2.2
validHaskellFileNames = sort [
    "pure.hs",
    "best.lhs",
    "good.better.hs",
    "      pure.hs      ",
    "pure.hs      ",
    "      pure.hs"
  ]
  
invalidHaskellFileNames = sort [
    "impure.c",
    "bad.java",
    "awesome.Hs",
    "pure.hs.txt",
    "cool.hs.txt"
  ]
  
allFileNames = (concat.transpose) [validHaskellFileNames, invalidHaskellFileNames]

testHaskellFileNames = all (==validHaskellFileNames) $ map (sort.haskellFileNames) inputSet
  where inputSet = take 10000 $ permutations allFileNames

-- 2.3
testSelect = and [
    select even [1..26] "abcdefghijklmnopqrstuvwxyz" == "bdfhjlnprtvxz",
    select odd  [1..26] "abcdefghijklmnopqrstuvwxyz" == "acegikmoqsuwy"
  ]

-- 2.4
testPrefixSum = and [
    prefixSum [1..10] == [1,3,6,10,15,21,28,36,45,55],
    prefixSum [10,9..1] == [10,19,27,34,40,45,49,52,54,55]
  ]

-- 2.5
testNumbers = and [
    numbers [1..4] == 1234,
    numbers ([1..9] ++ (0:[9,8..1])) == 1234567890987654321
  ]

-- 2.6
type Numeral = (Int, [Int])
example = (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0])

testNumeral = and [
    testMakeLongInt, 
    testEvaluateLongInt, 
    testChangeRadixLongInt, 
    testAddLongInts, 
    testMulLongInts
  ]

-- 2.6 1
testMakeLongInt = and [
    makeLongInt 123 10 == (10, [1,2,3]), 
    makeLongInt 12345678901234567890 10 == example,
    and [(makeLongInt ((toInteger r)^(toInteger p)) r) == (r, 1 : replicate p 0) | r<-[2..100], p<-[0..10]]
  ]

-- 2.6 2
testEvaluateLongInt = and [
    evaluateLongInt (10, [1,2,3]) == 123, 
    evaluateLongInt example == 12345678901234567890
  ]

-- 2.6 3
changeRadixLongIntSpec :: Numeral -> Int -> Numeral
changeRadixLongIntSpec n r = makeLongInt (evaluateLongInt n) r

testChangeRadixLongInt = and [
    changeRadixLongInt (10, [1,2,3]) 8 == (8, [1,7,3]), 
    changeRadixLongInt (10, [1,2,3]) 16 == (16, [7,11]),
    changeRadixLongInt (16, [13,14,10,13,11,14,14,15]) 17 == (17, [9,1,13,3,6,16,7,8])
  ]

-- 2.6 4
addLongIntsSpec :: Numeral -> Numeral -> Numeral
addLongIntsSpec (r1, ds1) (r2, ds2)
  | r1 == r2 = makeLongInt (evaluateLongInt (r1, ds1) + evaluateLongInt (r2, ds2)) r1
  | r1 < r2 = addLongIntsSpec (changeRadixLongIntSpec (r1, ds1) r2) (r2, ds2)
  | r1 > r2 = addLongIntsSpec (r1, ds1) (changeRadixLongIntSpec (r2, ds2) r1)

testAddLongInts = and [
    addLongInts (10, [1,2,3]) (3, [1]) == (10, [1,2,4]),
    addLongInts (16, [13,14,10,13,11,14,14,15]) (8, [7, 7, 7]) == (16, [13,14,10,13,12,0,14,14])
  ]

-- 2.6 5
mulLongIntsSpec :: Numeral -> Numeral -> Numeral
mulLongIntsSpec (r1, ds1) (r2, ds2)
  | r1 == r2 = makeLongInt (evaluateLongInt (r1, ds1) * evaluateLongInt (r2, ds2)) r1
  | r1 < r2 = mulLongIntsSpec (changeRadixLongIntSpec (r1, ds1) r2) (r2, ds2)
  | r1 > r2 = mulLongIntsSpec (r1, ds1) (changeRadixLongIntSpec (r2, ds2) r1)

testMulLongInts = and [
    mulLongInts (10, [1,2,3]) (3, [1]) == (10, [1,2,3]),
    mulLongInts (16, [13,14,10,13,11,14,14,15]) (8, [7, 7, 7]) == (16, [1,11,12,7,12,13,0,1,15,1,1])
  ]
