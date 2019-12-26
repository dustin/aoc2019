{-# LANGUAGE OverloadedStrings #-}

module Day25Tests where

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import           Data.List         (isInfixOf)

import           Day25

testPart1 :: Assertion
testPart1 = do
  rv <- brutus . weighMe <$> getInput
  assertBool rv ("262848" `isInfixOf` rv)

tests :: [TestTree]
tests = [
  testCase "part1" testPart1
  ]
