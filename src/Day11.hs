{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day11 where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           AoC
import           ComputerST
import           TwoD
import           Vis

getInput :: IO Instructions
getInput = readInstructions "input/day11"

runOne :: Int -> Instructions -> Map Point Int
runOne start prog = go (0,0) N mempty (executeIn [start] prog)
  where
    go _ _ m (Right _) = m
    go pos dir m (Left (NoInput p@Paused{..})) =
      let [c,t] = pausedOuts
          m' = Map.insert pos c m
          dir' = turn t dir
          pos' = fwd dir' pos in
        go pos' dir' m' (resume [Map.findWithDefault 0 pos' m] p)

    turn 0 = pred'
    turn _ = succ'

displayMap :: Map Point Int -> String
displayMap m = drawString m' cf
  where m' = fmap n m
        n 1 = '*'
        n _ = ' '
        cf x = Map.findWithDefault ' ' x m'

part1 :: IO Int
part1 = length . runOne 0 <$> getInput

part2 :: IO String
part2 = displayMap . runOne 1 <$> getInput
