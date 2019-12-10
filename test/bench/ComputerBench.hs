{-# LANGUAGE TemplateHaskell #-}

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

tests :: [Benchmark]
tests = [
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
