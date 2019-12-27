module Day23Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import           Data.Either     (fromRight)

import           Day23

srcFile :: FilePath
srcFile = "input/day23"

tests :: [Benchmark]
tests = [
  env getInput $ \ ~x -> bgroup "" [
      bench "part1" $ nf part1 x,
      bench "part1" $ nf part2 x
      ]
  ]
