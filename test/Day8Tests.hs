{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Day8Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import qualified Data.Map.Strict   as Map

import           Day8

testPart1 :: Assertion
testPart1 = assertEqual "" 2250 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" "0110" (flatten 2 2 "0222112222120000")

tests :: [TestTree]
tests = [
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2
  ]
