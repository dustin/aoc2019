{-# LANGUAGE OverloadedStrings #-}

module Day15Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Day15

-- Oxygen is at (18,-20)
testPart1 :: Assertion
testPart1 = assertEqual "" (Just 330) =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 352 =<< part2

tests :: [TestTree]
tests = [
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2
  ]
