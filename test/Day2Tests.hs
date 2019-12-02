module Day2Tests where

import qualified Data.Vector.Unboxed   as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Computer
import           Day2

testPart1 :: Assertion
testPart1 = assertEqual "" 3931283 =<< part1 <$> getInput

testPart2 :: Assertion
testPart2 = assertEqual "" 6979 =<< part2 <$> getInput

testPart2' :: Assertion
testPart2' = assertEqual "" 6979 =<< part2 <$> getInput

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2,
  testCase "part2'" testPart2'
  ]
