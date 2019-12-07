{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7Tests where

import           Data.Either           (fromRight)
import qualified Data.Vector.Unboxed   as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC


import           AoC
import           Computer
import           Day7

ex1 :: Instructions
ex1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]

ex2 :: Instructions
ex2 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

ex3 :: Instructions
ex3 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
       -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
       53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

testExample1 :: Assertion
testExample1 = assertEqual "" (Right 43210) (findMaxThrust ex1)

testPart1 :: Assertion
testPart1 = assertEqual "" (Right 43812) =<< part1

testExample2 :: Assertion
testExample2 = assertEqual "" [139629729] (findFeedback ex2)

testExample3 :: Assertion
testExample3 = assertEqual "" [18216] (findFeedback ex3)

-- > 58579570
testPart2 :: Assertion
testPart2 = assertEqual "" [59597414] =<< part2

tests :: [TestTree]
tests = [
  testCase "Ex1" testExample1,
  testCase "Part 1" testPart1,
  testCase "Ex2" testExample2,
  testCase "Ex3" testExample3,
  testCase "Part 2" testPart2
  ]