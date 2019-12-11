{-# LANGUAGE TemplateHaskell #-}

module Day10Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import           Data.Either     (fromRight)

import           Day10

instance NFData World where
  rnf (World m) = m `seq` ()

srcFile :: FilePath
srcFile = "input/day10"

input :: IO World
input = getInput srcFile

tests :: [Benchmark]
tests = [
  env input $ \ ~w -> bgroup "" [
      bench "core matches" $ nf (flip sees (37,25)) w,
      bench "sweep" $ nf (`sweep` (37,25)) w,
      bench "best" $ nf best w,
      bench "best (par)" $ nf bestPar w
  ]
  ]
