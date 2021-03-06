{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import           Computer
import qualified ComputerST as CST

getInput :: IO (Instructions Int)
getInput = readInstructions "input/day9"


part1 :: IO (Either (Termination Int) Int)
part1 = do
  prog <- getInput
  pure $ head . outputs <$> executeIn [1] prog

part2 :: IO (Either (Termination Int) Int)
part2 = do
  prog <- getInput
  pure $ head . outputs <$> executeWithinIns 1000000 [2] prog

part2ST :: IO (Either CST.Termination Int)
part2ST = do
  prog <- CST.readInstructions "input/day9"
  pure $ head . CST.outputs <$> CST.executeWithinIns 1000000 [2] prog
