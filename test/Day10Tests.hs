{-# LANGUAGE OverloadedStrings #-}

module Day10Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Day10

testPart1 :: Assertion
testPart1 = assertEqual "" ((37,25),309) =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 0 =<< part2

testTiny  :: Assertion
testTiny = assertEqual "" ((3,4),8) =<< best <$> getInput "input/day10.small"

testSmall  :: Assertion
testSmall = assertEqual "" ((5,8),33) =<< best <$> getInput "input/day10.small2"

testLarge  :: Assertion
testLarge = assertEqual "" ((11,13),210) =<< best <$> getInput "input/day10.large"

tests :: [TestTree]
tests = [
  testCase "tiny" testTiny,
  testCase "small" testSmall,
  testCase "large" testLarge,
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2
  ]
