module Main (main) where

import           Criterion      (bgroup)
import           Criterion.Main (defaultMain)

import           ComputerBench
import           Day10Bench
import           Day15Bench
import           Day4Bench

main :: IO ()
main = defaultMain [
  bgroup "computer" ComputerBench.tests,
  bgroup "day4" Day4Bench.tests,
  bgroup "day10" Day10Bench.tests,
  bgroup "day15" Day15Bench.tests
  ]
