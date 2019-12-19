{-# LANGUAGE OverloadedStrings #-}

module Day18Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day18

testPart2 :: Assertion
testPart2 = assertEqual "" 2086 =<< part2

test1 :: Int -> FilePath -> Assertion
test1 want fp = do
  n <- do1dijk fp
  assertEqual "" want n

test2 :: Int -> FilePath -> Assertion
test2 want fp = do
  n <- do2 fp
  assertEqual "" want n

tests :: [TestTree]
tests = [
  testCase "part1" (test1 4590 "input/day18")
  , testCase "small1" (test1 8 "input/day18.small")
  , testCase "small2" (test1 86 "input/day18.sample2")
  , testCase "small3" (test1 132 "input/day18.sample3")
  , testCase "small5" (test1 81 "input/day18.sample5")
  -- , testCase "small4" (test1 136 "input/day18.sample4") -- still slow
  -- , testCase "part2" testPart2
  , testCase "small1 2" (test2 8 "input/day18.smallb")
  , testCase "larger 2" (test2 32 "input/day18.larger")
  , testCase "largest" (test2 72 "input/day18.large")
  ]
