{-# LANGUAGE OverloadedStrings #-}

module Day23Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Day23

testPart1 :: Assertion
testPart1 = assertEqual "" 26464 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 19544 =<< part2

tests :: [TestTree]
tests = [
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2
  ]
