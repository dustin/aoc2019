{-# LANGUAGE OverloadedStrings #-}

module Day19Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day19

testPart1 :: Assertion
testPart1 = assertEqual "" 114 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 10671712 =<< part2

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
