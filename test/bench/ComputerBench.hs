{-# LANGUAGE OverloadedLists #-}

module ComputerBench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import           Data.Either     (fromRight)

import qualified Computer        as C
import qualified ComputerST      as CST

instance NFData (C.Instructions a)  where
  rnf (C.Instructions a) = a `seq` ()

srcFile :: FilePath
srcFile = "input/day9"

testProgInt :: IO (C.Instructions Int)
testProgInt = C.readInstructions srcFile

testProgInteger :: IO (C.Instructions Integer)
testProgInteger = C.readInstructions srcFile

testProgST :: IO CST.Instructions
testProgST = CST.readInstructions srcFile

sumOfPrimes :: CST.Instructions
sumOfPrimes = [3,100,1007,100,2,7,1105,-1,87,1007,100,1,14,1105,-1,27,101,-2,100,100,101,1,101,101,1105,1,9,101,105,101,105,101,2,104,104,101,1,102,102,1,102,102,103,101,1,103,103,7,102,101,52,1106,-1,87,101,105,102,59,1005,-1,65,1,103,104,104,101,105,102,83,1,103,83,83,7,83,105,78,1106,-1,35,1101,0,1,-1,1105,1,69,4,104,99]

ackerman :: CST.Instructions
ackerman = [109,99,21101,0,13,0,203,1,203,2,1105,1,16,204,1,99,1205,1,26,22101,1,2,1,2105,1,0,1205,2,40,22101,-1,1,1,21101,0,1,2,1105,1,16,21101,0,57,3,22101,0,1,4,22101,-1,2,5,109,3,1105,1,16,109,-3,22101,0,4,2,22101,-1,1,1,1105,1,16]

isqrt :: CST.Instructions
isqrt = [3,1,109,149,21101,0,15,0,20101,0,1,1,1105,1,18,204,1,99,22101,0,1,2,22101,0,1,1,21101,0,43,3,22101,0,1,4,22101,0,2,5,109,3,1105,1,78,109,-3,22102,-1,1,1,22201,1,4,3,22102,-1,1,1,1208,3,0,62,2105,-1,0,1208,3,1,69,2105,-1,0,22101,0,4,1,1105,1,26,1207,1,1,83,2105,-1,0,21101,0,102,3,22101,0,2,4,22101,0,1,5,109,3,1105,1,115,109,-3,22201,1,4,1,21101,0,2,2,1105,1,115,2102,-1,2,140,2101,0,2,133,22101,0,1,2,20001,133,140,1,1207,2,-1,136,2105,-1,0,21201,2,-1,2,22101,1,1,1,1105,1,131]

divm :: CST.Instructions
divm = [109,366,21101,0,13,0,203,1,203,2,1105,1,18,204,1,204,2,99,1105,0,63,101,166,19,26,1107]

factor :: CST.Instructions
factor = [3,1,109,583,108,0,1,9,1106,-1,14,4,1,99,107,0,1,19,1105,-1,27,104,-1,102,-1,1,1,21101,0,38,0,20101,0,1,1,1105,1,138,2101,1,1,41,101,596,41,45,1101,1,596,77,1101,0,1,53,101,1,77,77,101,1,53,53,7,45,77,67,1105,-1,128,108,1,1,74,1105,-1,128,1005,-1,54,1,53,77,93,7,45,93,88,1105,-1,101,1101,0,1,-1,1,53,93,93,1105,1,83,21101,0,116,0,20101,0,1,1,20101,0,53,2,1105,1,235,1205,2,54,4,53,2101,0,1,1,1105,1,101,108,1,1,133,1105,-1,137,4,1,99,22101,0,1,2,22101,0,1,1,21101,0,163,3,22101,0,1,4,22101,0,2,5,109,3,1105,1,198,109,-3,22102,-1,1,1,22201,1,4,3,22102,-1,1,1,1208,3,0,182,2105,-1,0,1208,3,1,189,2105,-1,0,22101,0,4,1,1105,1,146,1207,1,1,203,2105,-1,0,21101,0,222,3,22101,0,2,4,22101,0,1,5,109,3,1105,1,235,109,-3,22201,1,4,1,21101,0,2,2,1105,1,235,1105,0,280,101,383,236,243,1107,-1,583,247,1106,-1,276,101,383,236,256,102,1,275,-1,102,2,275,275,1007,275,0,266,1105,-1,280,101,1,236,236,1105,1,238,1,101,-1,236,236,101,383,236,286,207,1,-1,289,1106,-1,-1,22101,0,1,3,2102,1,2,363,2102,-1,2,369,22102,0,1,1,22102,0,2,2,101,1,236,320,101,-1,320,320,1107,-1,0,324,2105,-1,0,22102,2,2,2,101,383,320,336,207,3,-1,339,1105,-1,361,22101,1,2,2,22102,-1,3,3,101,383,320,354,22001,-1,3,3,22102,-1,3,3,1207,2,-1,366,1105,-1,315,22101,-1,2,2,101,383,320,377,22001,-1,1,1,1105,1,315]

rbench :: [Int] -> CST.Instructions -> [Int]
rbench ins = either (error . show) CST.outputs . CST.executeWithinIns 100000000 ins

tests :: [Benchmark]
tests = [
  bgroup  "reddit" [
      bench "sum-of-primes 100000" $ nf (rbench [100000]) sumOfPrimes,
      bench "ackerman 3,6" $ nf (rbench [3,5]) ackerman,
      bench "isqrt 130" $ nf (rbench [130]) isqrt,
      -- bench "divmod 1024, 3" $ nf (rbench [1024, 3]) divm,
      bench "factor 19338240" $ nf (rbench [19338240]) factor,
      bench "factor 2147483647" $ nf (rbench [2147483647]) factor
      ],
  bgroup "mine" [
      env testProgInt $ \ ~s -> bgroup "map int" [
          bench "d9 part2" $ nf (C.outputs . fromRight undefined . C.executeWithinIns 1000000 [2]) s
          ],
      env testProgInteger $ \ ~s -> bgroup "map integer" [
          bench "d9 part2" $ nf (C.outputs . fromRight undefined . C.executeWithinIns 1000000 [2]) s
          ],
      env testProgST $ \ ~s -> bgroup "ST" [
          bench "d9 part2" $ nf (CST.outputs . fromRight undefined . CST.executeWithinIns 1000000 [2]) s
          ]
      ]
  ]
