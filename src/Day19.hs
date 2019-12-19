{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day19 where

import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe, listToMaybe)
import           Data.Monoid                 (Sum (..))

import           ComputerST
import           Search
import           Vis

getInput :: IO Instructions
getInput = readInstructions "input/day19"

runOne :: [Int] -> Instructions -> [Int]
runOne ins prog = case executeIn ins prog of
                    Right (FinalState{outputs}) -> outputs
                    Left x -> error ("Unexpected termination: " <> show x)

extractOne :: Instructions -> (Int, Int) -> Int
extractOne prog (x,y) = head $ runOne [x, y] prog

extractGrid :: Instructions -> ((Int,Int), (Int,Int)) -> Map (Int,Int) Int
extractGrid prog ((mnx,mny), (mxx, mxy)) = Map.fromList . zip xys $!
                                           parMap rdeepseq (extractOne prog) $! xys
  where xys = [(x,y) | x <- [mnx..mxx], y <- [mny..mxy]]

displayGrid :: Map (Int,Int) Int -> String
displayGrid m = drawString m (mapCharFun m cf)
  where cf 0 = '.'
        cf 1 = 'O'

pngGrid :: FilePath -> Map (Int, Int) Int -> IO ()
pngGrid fn g = draw fn g (mapPixelFun g c)
  where c 0 = black
        c 1 = green

part1 :: IO Int
part1 = do
  prog <- getInput
  pure $ getSum . foldMap Sum $ extractGrid prog ((0,0),(49,49))

searchFull :: Instructions -> (Int, Int)
searchFull prog = let yout = head [ y | y <- [close - 25..], isFull (xoff y, y)]
                  in (xoff yout, yout-99)
  where
    close = autoBinSearch t

    isFull (x,y) = and [at (x+xo, y-yo) | xo <- [0,99], yo <- [0,99]]

    at = (== 1) . extractOne prog

    xoff y = binSearch (\x -> if at (x,y) then GT else LT) 0 upper
      where upper = fromMaybe 0 $ listToMaybe [x | x <- [0, (y `div` 50) .. y], at (x,y)]

    t y
      | y < 100 = LT
      | otherwise = if isFull (xoff y, y) then GT else LT

tryOne :: Int -> Int -> IO ()
tryOne x y = do
  prog <- getInput
  putStrLn $ displayGrid $ extractGrid prog ((x-3,y-3),(x+105,y+105))

part2 :: IO Int
part2 = do
  prog <- getInput
  let (x,y) = searchFull prog
  pure $ x * 10000 + y
