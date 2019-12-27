{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import           Data.List.Extra (chunksOf, minimumOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Vis

getInput :: IO String
getInput = filter (/= '\n') <$> readFile "input/day8"

myWidth, myHeight :: Int
myWidth = 25
myHeight = 6

part1 :: IO Int
part1 = do
  layers <- chunksOf (myWidth*myHeight) <$> getInput
  let v = minimumOn (count '0') layers
  pure (count '1' v * count '2' v)
    where
      count x = length . filter (== x)

layerMap :: Int -> Int -> String -> Map (Int,Int) Char
layerMap w h = Map.fromList . zip [(x,y) | y <- [0.. h - 1], x <- [0.. w - 1]]

flatten :: Int -> Int -> String -> String
flatten w h = foldr (zipWith j) (repeat '2') . chunksOf (w*h)
  where
    j '2' x = x
    j x '2' = x
    j x _   = x

-- FHJUL
part2 :: IO ()
part2 = do
  layers <- getInput
  let flat = flatten myWidth myHeight layers
      m = layerMap myWidth myHeight flat

  putStrLn (drawString m (mapCharFunTrans m [('0', ' '), ('1', '▮')]))

  draw "day8.png" m (mapPixelFunTrans m [('0', white), ('1', black)])
