{-# LANGUAGE OverloadedStrings #-}

module Day12Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Day12

testPart1 :: Assertion
testPart1 = assertEqual "" 7722 =<< part1

testPart2small :: Assertion
testPart2small = assertEqual "" 4686774924 =<< part2On "input/day12.sample2"

testPart2 :: Assertion
testPart2 = assertEqual "" 292653556339368 =<< part2

tests :: [TestTree]
tests = [
  testCase "Part 1" testPart1,
  testCase "Part 2 small" testPart2small
  -- testCase "Part 2" testPart2
  ]
