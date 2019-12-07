{-# LANGUAGE TemplateHaskell #-}

module Day4Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)

import           Day4

genCount "answer1" (100000, 999999) $ any (> 1)
genCount "answer2" (100000, 999999) (2 `elem`)

allpart1 :: () -> Int
allpart1 _ = answer1

allpart2 :: () -> Int
allpart2 _ = answer2

tests :: [Benchmark]
tests = [
  bench "part1" $ nf allpart1 (),
  bench "part2" $ nf allpart2 ()
  ]
