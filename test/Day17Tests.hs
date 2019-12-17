{-# LANGUAGE OverloadedStrings #-}

module Day17Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day17

testPart1 :: Assertion
testPart1 = assertEqual "" 6024 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 897344 =<< part2

tests :: [TestTree]
tests = [
  testCase "Part 1" testPart1
  , testCase "Part 2" testPart2
  ]
