{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import           Control.Monad               (foldM)
import           Control.Parallel.Strategies
import           Data.List                   (permutations)

import           Computer

getInput :: IO (Instructions Int)
getInput = readInstructions "input/day7"

findMaxThrust :: Instructions Int -> Either (Termination Int) Int
findMaxThrust prog = fmap maximum <$> traverse runList $ permutations [0..4]
  where
    runList = foldM (flip runOnce) 0
    runOnce a b = head . outputs <$> executeIn [a,b] prog

-- This was: `fmap maximum <$> traverse run $ permutations [5..9]` but
-- I haven't been able to get that to parallelize well.
findFeedback :: Instructions Int -> Int
findFeedback prog = maximum . fmap maximum . sequenceA . parMap rseq run $ permutations [5..9]
  where
    run = next . start . seed
    seed (x:xs) = [[x,0]] <> map (:[]) xs
    seed []     = error "empty seed"
    start = zip ['A'..] . map (`executeIn` prog)
    next (('E', Right x):_) = outputs x
    next (x:xs)             = forward x xs
    next []                 = error "empty next"
    forward (_, Right fs) ((p2, Left (NoInput p)):xs) = next $ (p2, resume p{pausedOuts=[]} (outputs fs)) : xs
    forward (p1, Left (NoInput pa1)) ((p2, Left (NoInput pa2)):xs) =
      next $ (p2, resume pa2{pausedOuts=[]} (pausedOuts pa1)) : xs <> [(p1, Left (NoInput pa1{pausedOuts=[]}))]
    forward x xs = error ("Unhandled forward: " <> show x <> " : " <> show xs)

part1 :: IO (Either (Termination Int) Int)
part1 = findMaxThrust <$> getInput

part2 :: IO Int
part2 = findFeedback <$> getInput
