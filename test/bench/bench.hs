module Main (main) where

import           Criterion      (bgroup)
import           Criterion.Main (defaultMain)

import           Day4Bench

main :: IO ()
main = defaultMain [
  bgroup "day4" Day4Bench.tests
  ]
