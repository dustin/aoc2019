{-# LANGUAGE OverloadedStrings #-}

module Day3Tests where

import           Data.Text
import qualified Data.Vector.Unboxed   as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           AoC
import           Day3

parseEx :: Text -> [Wire]
parseEx = parseLit parseAll

testEx :: Assertion
testEx = mapM_ (\(t, want) -> assertEqual (show t) want (part1 . parseEx $ t)) [
  ("R8,U5,L5,D3\nU7,R6,D4,L4\n", 6),
  ("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83\n", 159),
  ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7\n", 135)
  ]

testPart1 :: Assertion
testPart1 = assertEqual "" 280 =<< part1 <$> getInput

testEx2 :: Assertion
testEx2 = assertEqual "" 30 (part2 . parseEx $ "R8,U5,L5,D3\nU7,R6,D4,L4\n")

testPart2 :: Assertion
testPart2 = assertEqual "" 10554 =<< part2 <$> getInput

tests :: [TestTree]
tests = [
  testCase "ex1" testEx,
  testCase "ex2" testEx2,

  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
