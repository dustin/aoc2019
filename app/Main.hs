module Main where

import qualified Day1

main :: IO ()
main = print =<< Day1.part1 <$> Day1.getInput
