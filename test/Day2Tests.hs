module Day2Tests where

import qualified Data.Vector.Unboxed   as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day2

testPart1 :: Assertion
testPart1 = assertEqual "" 3931283 =<< part1 <$> getInput

testPart2 :: Assertion
testPart2 = assertEqual "" 6979 =<< part2 <$> getInput

testEx :: Assertion
testEx = assertEqual "" 3500 (V.head $ execute 0 (V.fromList [1,9,10,3,2,3,11,0,99,30,40,50]))

tests :: [TestTree]
tests = [
  testCase "ex" testEx,
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
