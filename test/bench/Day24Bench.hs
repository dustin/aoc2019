module Day24Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import           Data.Either     (fromRight)

import           Day24

srcFile :: FilePath
srcFile = "input/day24"

tests :: [Benchmark]
tests = [
  env (getInput srcFile) $ \ ~x -> bench "part1" $ nf part1 x,
  env (upgrade <$> getInput srcFile) $ \ ~x -> bench "part2" $ nf (part2' 200) x
  ]
