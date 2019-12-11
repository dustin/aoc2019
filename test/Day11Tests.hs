{-# LANGUAGE OverloadedStrings #-}

module Day11Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Day11

testPart1 :: Assertion
testPart1 = assertEqual "" 2339 =<< part1

twoout :: String
twoout = " ***   **  *  * **** ***  *    ***  ***    \n *  * *  * *  * *    *  * *    *  * *  *   \n *  * *    *  * ***  *  * *    *  * *  *   \n ***  * ** *  * *    ***  *    ***  ***    \n *    *  * *  * *    *    *    *    * *    \n *     ***  **  **** *    **** *    *  *   "

testPart2 :: Assertion
testPart2 = assertEqual "" twoout =<< part2

tests :: [TestTree]
tests = [
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2
  ]
