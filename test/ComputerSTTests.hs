{-# LANGUAGE OverloadedLists #-}

module ComputerSTTests where

import           Data.Either           (fromRight)
import qualified Data.Vector.Unboxed   as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           ComputerST

d1ex :: Instructions
d1ex = [1,9,10,3,2,3,11,0,99,30,40,50]

testD2Ex :: Assertion
testD2Ex = assertEqual "" 3500 (V.head $ ram . fromRight undefined $ execute d1ex)

testTimeout :: Assertion
testTimeout = assertEqual "" (Left $ Bugger "timed out") (executeWithin 2 d1ex)

d5ex :: Instructions
d5ex = [3,0,4,0,99]

d5ex2 :: Instructions
d5ex2 = [1002,4,3,4,33]

testD5ex1 :: Assertion
testD5ex1 = assertEqual "" [5150] (outputs . fromRight undefined $ executeIn [5150] d5ex)

testD5ex1L :: Assertion
testD5ex1L = assertEqual "" [5150] $ lrun d5ex [5150]
  where
    lrun prog ins = outputs . fromRight undefined . executeIn ins $ prog

testD5ex2 :: Assertion
testD5ex2 = assertEqual "" [1002,4,3,4,99] (V.take 5 . ram . fromRight undefined $ execute d5ex2)

testD5Compares :: Assertion
testD5Compares = mm [
  (ex1, 8, 1), (ex1, 9, 0),
  (ex2, 5, 1), (ex2, 9, 0),
  (ex3, 8, 1), (ex3, 9, 0),
  (ex4, 5, 1), (ex4, 9, 0)
  ]
  where
    mm :: [(Instructions, Int, Int)] -> Assertion
    mm = mapM_ aProg
    run :: Instructions -> Int -> Int
    run prog num = (head . outputs . fromRight undefined $ executeIn [num] prog)
    aProg :: (Instructions, Int, Int) -> Assertion
    aProg (prog, num, want) = assertEqual (show prog <> "@" <> show num) want (run prog num)
    ex1 :: Instructions
    ex1 = [3,9,8,9,10,9,4,9,99,-1,8]
    ex2 :: Instructions
    ex2 = [3,9,7,9,10,9,4,9,99,-1,8]
    ex3 :: Instructions
    ex3 = [3,3,1108,-1,8,3,4,3,99]
    ex4 :: Instructions
    ex4 = [3,3,1107,-1,8,3,4,3,99]


testD5Jumps :: Assertion
testD5Jumps = mm [
  (ex1, 0, 0), (ex1, 9, 1),
  (ex2, 0, 0), (ex2, 9, 1)
  ]
  where
    mm :: [(Instructions, Int, Int)] -> Assertion
    mm = mapM_ aProg
    run :: Instructions -> Int -> Int
    run prog num = (head . outputs . fromRight undefined $ executeIn [num] prog)
    aProg :: (Instructions, Int, Int) -> Assertion
    aProg (prog, num, want) = assertEqual (show prog <> "@" <> show num) want (run prog num)
    ex1 :: Instructions
    ex1 = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
    ex2 :: Instructions
    ex2 = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]


d5ex3 :: Instructions
d5ex3 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
         1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
         999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

testD5OMG :: Assertion
testD5OMG = assertEqual "" [999] (outputs . fromRight undefined $ executeIn [7] d5ex3)

testD5OMG2 :: Assertion
testD5OMG2 = assertEqual "" [1000] (outputs . fromRight undefined $ executeIn [8] d5ex3)

testD5OMG3 :: Assertion
testD5OMG3 = assertEqual "" [1001] (outputs . fromRight undefined $ executeIn [11] d5ex3)

d9quineEx :: Instructions
d9quineEx = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

testD9Copy :: Assertion
testD9Copy = assertEqual "" [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] (
  outputs . fromRight undefined $ execute d9quineEx)

d9bignumEx :: Instructions
d9bignumEx = [1102,34915192,34915192,7,4,7,99,0]

testD9bignum :: Assertion
testD9bignum = assertEqual "" [1219070632396864] (outputs . fromRight undefined $ executeIn [] d9bignumEx)

d9bigOut :: Instructions
d9bigOut = [104,1125899906842624,99]

testD9BigOut :: Assertion
testD9BigOut = assertEqual "" [1125899906842624] (outputs . fromRight undefined $ executeIn [] d9bigOut)

tests :: [TestTree]
tests = [
  testCase "Day 2 Example" testD2Ex,
  testCase "timeout" testTimeout,
  testCase "day 5 simple" testD5ex1,
  testCase "day 5 simple (lrun)" testD5ex1L,
  testCase "day 5 ex 2" testD5ex2,
  testCase "day 5 jump < 8" testD5OMG,
  testCase "day 5 jump == 8" testD5OMG2,
  testCase "day 5 jump > 8" testD5OMG3,
  testCase "day 5 compares" testD5Compares,
  testCase "day 5 jumps" testD5Jumps,
  testCase "day 9 copy ex" testD9Copy,
  testCase "day 9 bignum" testD9bignum,
  testCase "day 9 big out" testD9BigOut
  ]
