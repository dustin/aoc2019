{-# LANGUAGE OverloadedStrings #-}

module Day14Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Day14

-- < 525164
-- < 479447
-- == 431448

testPart1 :: Assertion
testPart1 = assertEqual "" 431448 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 3279311 =<< part2

test1Small :: Assertion
test1Small = assertEqual "" 31 =<< ((`oreReq` 1) <$> getInput "input/day14.small")

test2Small :: Assertion
test2Small = assertEqual "" 165 =<< ((`oreReq` 1) <$> getInput "input/day14.small2")

test1Large :: Assertion
test1Large = assertEqual "" 13312 =<< ((`oreReq` 1) <$> getInput "input/day14.large")

tests :: [TestTree]
tests = [
  testCase "small 1" test1Small,
  testCase "small 2" test2Small,
  testCase "large 1" test1Large,
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2
  ]
