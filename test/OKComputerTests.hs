module OKComputerTests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           OKComputer


testOps :: Assertion
testOps = assertEqual "" [(1,OpAdd),(2,OpMul),(3,OpIn),
                          (4,OpOut),(5,OpJT),(6,OpJF),
                          (7,OpLT),(8,OpEq),(9,OpSetrel),
                          (10,OpHalt)] [(x, idOP x) | x <- [1..10]]

testModes :: Assertion
testModes =  mm [(4, (Position, Position, Position)),
                 (1002, (Position, Immediate, Position)),
                 (10002, (Position, Position, Immediate)),
                 (22202, (Relative, Relative, Relative))
                 ]
  where
    mm :: [(Integer, Modes)] -> Assertion
    mm = mapM_ one
    one (num, want) = assertEqual (show num) want (snd . decodeInstruction $ num)

tests :: [TestTree]
tests = [
  testCase "ops" testOps,
  testCase "modes" testModes
  ]
