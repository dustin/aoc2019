{-# LANGUAGE OverloadedStrings #-}

module Day3Tests where

import           Data.Text
import qualified Data.Vector.Unboxed   as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Text.Megaparsec       (Parsec, parse)
import           Text.Megaparsec.Error (errorBundlePretty)

import           AoC
import           Day3

parseEx :: Text -> [Wire]
parseEx s = either (fail.errorBundlePretty) id (parse parseAll "" s)

testEx :: Assertion
testEx = assertEqual "" 6 (part1 . parseEx $ "R8,U5,L5,D3\nU7,R6,D4,L4\n")

testEx2 :: Assertion
testEx2 = assertEqual "" 159 (part1 . parseEx $ "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83\n")

testEx3 :: Assertion
testEx3 = assertEqual "" 135 (part1 . parseEx $ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7\n")

testPart1 :: Assertion
testPart1 = assertEqual "" 280 =<< part1 <$> getInput

testEx3_2 :: Assertion
testEx3_2 = assertEqual "" 30 (part2 . parseEx $ "R8,U5,L5,D3\nU7,R6,D4,L4\n")

-- too low: 10552
testPart2 :: Assertion
testPart2 = assertEqual "" 10554 =<< part2 <$> getInput

tests :: [TestTree]
tests = [
  testCase "ex1" testEx,
  testCase "ex2" testEx2,
  testCase "ex3" testEx3,

  testCase "ex3 2" testEx3_2,

  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
