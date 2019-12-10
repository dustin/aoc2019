{-# LANGUAGE OverloadedStrings #-}

module Day9Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Day9

testPart1 :: Assertion
testPart1 = assertEqual "" (Right 3100786347) =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" (Right 87023) =<< part2

testPart2ST :: Assertion
testPart2ST = assertEqual "" (Right 87023) =<< part2ST


tests :: [TestTree]
tests = [
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2,
  testCase "Part 2 (ST)" testPart2ST
  ]
