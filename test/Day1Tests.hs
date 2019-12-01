module Day1Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day1

testPart1 :: Assertion
testPart1 = assertEqual "" 3317659 =<< part1 <$> getInput

testPart2 :: Assertion
testPart2 = assertEqual "" 4973616 =<< part2 <$> getInput

testPart2Ex :: Assertion
testPart2Ex = assertEqual "" 966 $ fuelFuelReq 1969

testPart2Ex2 :: Assertion
testPart2Ex2 = assertEqual "" 50346 $ fuelFuelReq 100756

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2 ex" testPart2Ex,
  testCase "part2 ex 2" testPart2Ex2,
  testCase "part2" testPart2
  ]
