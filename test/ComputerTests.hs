module ComputerTests where

import qualified Data.Vector.Unboxed   as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Computer

testD1Ex :: Assertion
testD1Ex = assertEqual "" 3500 (V.head $ execute (V.fromList [1,9,10,3,2,3,11,0,99,30,40,50]))

tests :: [TestTree]
tests = [
  testCase "Day 1 Example" testD1Ex
  ]
