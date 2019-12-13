{-# LANGUAGE OverloadedStrings #-}

module Day13Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Day13

testPart1 :: Assertion
testPart1 = assertEqual "" 273 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 13140 =<< score . head <$> part2

tests :: [TestTree]
tests = [
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2
  ]
