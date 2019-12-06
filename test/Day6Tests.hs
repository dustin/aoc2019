{-# LANGUAGE OverloadedStrings #-}

module Day6Tests where

import           Data.Either           (fromRight)
import qualified Data.Vector.Unboxed   as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC


import           AoC
import           Day6

getExample :: IO [Orbit]
getExample = parseFile parseAll "input/day6.sample"

getExample2 :: IO [Orbit]
getExample2 = parseFile parseAll "input/day6.sample2"

testPart1 :: Assertion
testPart1 = assertEqual "" 270768 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" (Just 451) =<< part2

testExample :: Assertion
testExample = assertEqual "" 42 =<< countOrbits <$> getExample

testExample2 :: Assertion
testExample2 = assertEqual "" (Just 4) =<< countTransfers "YOU" "SAN" <$> getExample2


tests :: [TestTree]
tests = [
  testCase "Example" testExample,
  testCase "Example 2" testExample2,
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2
  ]
