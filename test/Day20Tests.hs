{-# LANGUAGE OverloadedStrings #-}

module Day20Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day20

testPart1 :: Assertion
testPart1 = assertEqual "" 544 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 6238 =<< part2

test1 :: Int -> FilePath -> Assertion
test1 exp fp = do
  m <- getInput fp
  let (Just (n,_)) = findPath m
  assertEqual "" exp n

test2 :: Int -> FilePath -> Assertion
test2 exp fp = do
  m <- getInput fp
  let (Just n) = findPath2 m
  assertEqual "" exp n

tests :: [TestTree]
tests = [
  testCase "Part 1 small" (test1 23 "input/day20.example")
  , testCase "Part 1 large" (test1 58 "input/day20.example2")
  , testCase "Part 1" testPart1
  , testCase "Part 2 interesting" (test2 396 "input/day20.example3")
  -- , testCase "Part 2" testPart2 -- takes 5s
  ]
