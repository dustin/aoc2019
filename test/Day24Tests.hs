{-# LANGUAGE OverloadedStrings #-}

module Day24Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Day24

testPart1 :: Int -> FilePath -> Assertion
testPart1 exp fp = assertEqual "" exp =<< part1 <$> getInput fp

testPart2 :: Int -> Int -> FilePath -> Assertion
testPart2 exp mins fp = assertEqual "" exp =<< part2 mins <$> getInput fp

tests :: [TestTree]
tests = [
  testCase "Example 1" (testPart1 2129920 "input/day24.example"),
  testCase "Part 1" (testPart1 23967691 "input/day24"),
  testCase "Example 2" (testPart2 99 10 "input/day24.example"),
  testCase "Part 2" (testPart2 2003 200 "input/day24")
  ]
