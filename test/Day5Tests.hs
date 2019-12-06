module Day5Tests where

import           Data.Either           (fromRight)
import qualified Data.Vector.Unboxed   as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Computer
import           Day5

compute :: [Int] -> IO (Either Termination [Int])
compute myinput = do
  ins <- getInput
  pure $ outputs <$> executeIn myinput ins

testPart1 :: Assertion
testPart1 = assertEqual "" (Right 4511442) =<< (fmap last <$> compute [1])

testPart2 :: Assertion
testPart2 = assertEqual "" (Right [12648139]) =<< compute [5]

tests :: [TestTree]
tests = [
  testCase "Part 1" testPart1,
  testCase "Part 2" testPart2
  ]
