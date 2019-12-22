{-# LANGUAGE OverloadedStrings #-}

module Day21Tests where

import           Data.Word             (Word8)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day21

testPart1 :: Assertion
testPart1 = assertEqual "" (Right 19360724) =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" (Right 1140450681) =<< part2

instance Arbitrary Op where
  arbitrary = do
    c <- elements [AND, OR, NOT]
    c <$> arbitraryBoundedEnum <*> elements [T, J]

propOpWording :: Op -> Property
propOpWording op = opFromWord (opToWord op) === op

tests :: [TestTree]
tests = [
  testCase "Part 1 small" testPart1
  , testCase "Part 2" testPart2
  , testProperty "op wording" propOpWording
  ]
