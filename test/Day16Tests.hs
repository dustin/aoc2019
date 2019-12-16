{-# LANGUAGE OverloadedStrings #-}

module Day16Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day16

testPart1 :: Assertion
testPart1 = assertEqual "" 27831665 =<< part1

testPart2 :: Assertion
testPart2 = assertEqual "" 36265589 =<< part2

testPart2ST :: Assertion
testPart2ST = assertEqual "" 36265589 =<< (doPart2 <$> getInput)

propFFTAdd :: Int -> Property
propFFTAdd x = fftNaive v === fft v
  where
    v = parseNum . show . abs $ x

testPart2Small :: Assertion
testPart2Small = assertEqual "" 84462026 $ doPart2 (parseNum "03036732577212944063491565474664")

testPart2SmallST :: Assertion
testPart2SmallST = assertEqual "" 84462026 $ doPart2ST (parseNum "03036732577212944063491565474664")


tests :: [TestTree]
tests = [
  -- testProperty "fft add vs. naive" propFFTAdd
  -- , testCase "Part 1" testPart1
  -- testCase "Part 2 small" testPart2Small
  -- testCase "Part 2 small ST" testPart2SmallST
  -- , testCase "Part 2" testPart2
  --, testCase "Part 2 ST" testPart2ST
  ]
