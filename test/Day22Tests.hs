{-# LANGUAGE OverloadedStrings #-}

module Day22Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Data.Sequence     (Seq)
import qualified Data.Sequence     as Seq

import           Day22

testPart1 :: Assertion
testPart1 = assertEqual "" (Just 6061) =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 0 =<< part2

testDeals :: Assertion
testDeals = do
  let cards = Seq.fromList [0..9]

  assertEqual "new" (Seq.fromList [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]) $ apply [NewStack] cards
  assertEqual "cut+" (Seq.fromList [3, 4, 5, 6, 7, 8, 9, 0, 1, 2]) $ apply [Cut 3] cards
  assertEqual "cut-" (Seq.fromList [6, 7, 8, 9, 0, 1, 2, 3, 4, 5]) $ apply [Cut (-4)] cards
  assertEqual "inc" (Seq.fromList [0, 7, 4, 1, 8, 5, 2, 9, 6, 3]) $ apply [Increment 3] cards

test1 :: FilePath -> [Int] -> Assertion
test1 fp want = do
  inp <- getInput fp
  assertEqual "" (Seq.fromList want) $ apply inp (Seq.fromList [0..9])

tests :: [TestTree]
tests = [
  testCase "deals" testDeals,
  testCase "Part 1 small" (test1 "input/day22.small" [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]),
  testCase "Part 1 large" (test1 "input/day22.large" [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]),
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2
  ]
