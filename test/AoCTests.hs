module AoCTests where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit        hiding (assert)
import           Test.Tasty.QuickCheck   as QC

import qualified Data.Map.Strict         as Map

import           AoC

testParseGrid :: Assertion
testParseGrid = assertEqual "" (Map.fromList [((0,0), 'a'), ((1,0), 'b'),
                                              ((0,1), 'c'), ((1,1), 'd')]) $
                parseGrid id "ab\ncd\n"

tests :: [TestTree]
tests = [
  testCase "parse grid test" testParseGrid
  ]
