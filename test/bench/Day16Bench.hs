{-# LANGUAGE TemplateHaskell #-}

module Day16Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import           Data.Either     (fromRight)

import           Day16

srcFile :: FilePath
srcFile = "input/day16"

tests :: [Benchmark]
tests = [
  env getInput $ \ ~x -> bench "part2" $ nf (doPart2ST) x
  ]
