module Day4Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day4

input :: (Int, Int)
input = (264360, 746325)

testPart1 :: Assertion
testPart1 = assertEqual "" 945 (part1 input)

testPart2 :: Assertion
testPart2 = assertEqual "" 617 (part2 input)

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
