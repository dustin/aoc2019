{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import           Computer

getInput :: IO Instructions
getInput = readInstructions "input/day9"


part1 :: IO (Either Termination Integer)
part1 = do
  prog <- getInput
  pure $ head . outputs <$> executeIn [1] prog

part2 :: IO (Either Termination Integer)
part2 = do
  prog <- getInput
  pure $ head . outputs <$> executeWithinIns 1000000 [2] prog
