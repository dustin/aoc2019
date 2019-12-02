{-# LANGUAGE OverloadedLists #-}

module ComputerTests where

import           Data.Either           (fromRight)
import qualified Data.Vector.Unboxed   as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Computer

d1ex :: Instructions
d1ex = [1,9,10,3,2,3,11,0,99,30,40,50]

testD1Ex :: Assertion
testD1Ex = assertEqual "" 3500 (V.head $ fromRight undefined $ execute d1ex)

testTimeout :: Assertion
testTimeout = assertEqual "" (Left "timed out") (executeWithin 2 d1ex)

tests :: [TestTree]
tests = [
  testCase "Day 1 Example" testD1Ex,
  testCase "timeout" testTimeout
  ]
