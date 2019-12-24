{-# LANGUAGE OverloadedStrings #-}

module Day24Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Day24

testPart1 :: Int -> FilePath -> Assertion
testPart1 exp fp = assertEqual "" exp =<< part1 <$> getInput fp

testPart2 :: Assertion
testPart2 = assertEqual "" 0 =<< part2 <$> getInput "input/day24"

tests :: [TestTree]
tests = [
  testCase "Example 1" (testPart1 2129920 "input/day24.example"),
  testCase "Part 1" (testPart1 0 "input/day24"),
  testCase "Part 2" testPart2
  ]
