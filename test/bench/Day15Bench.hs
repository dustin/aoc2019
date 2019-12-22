module Day15Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)
import           Data.Either     (fromRight)

import           ComputerST
import           Day15

srcFile :: FilePath
srcFile = "input/day15"

progIn :: IO Instructions
progIn = getInput

worldIn :: IO World
worldIn = getInput >>= \prog ->  pure . world . snd . runSearch prog (const $ pure False) $ (0,0)

tests :: [Benchmark]
tests = [
  env progIn $ \ ~prog -> bgroup "instrs" [
      bench "part1" $ nf findPath prog
  ],
  env worldIn $ \ ~w -> bgroup "world" [
      bench "part2" $ nf (flood (18,-20)) w
      ]
  ]
