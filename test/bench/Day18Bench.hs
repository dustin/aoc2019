{-# LANGUAGE TemplateHaskell #-}

module Day18Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import           Data.Maybe      (fromJust)

import           Day18

srcFile :: FilePath
srcFile = "input/day18"

tests :: [Benchmark]
tests = [
  env (getInput srcFile) $ \ ~x -> bench "part1" $ nf (fst . fromJust . dijk) x
  ]
