{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import           Data.List.Extra (chunksOf)
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
  let lm = Map.fromList $ map (\x -> (count '0' x, x)) layers
      (_, v) = Map.findMin lm
  pure (count '1' v * count '2' v)
    where
      count x = length . filter (== x)

layerMap :: Int -> Int -> String -> Map (Int,Int) Char
layerMap w h = Map.fromList . zip [((x,y)) | y <- [0.. h - 1], x <- [0.. w - 1]]

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

  putStrLn (drawString m (\pos -> case m Map.! pos of
                                    '0' -> ' '
                                    '1' -> '▮'
                                    _   -> '?'))

  draw "day8.png" m (\pos -> case m Map.! pos of
                               '0' -> white
                               '1' -> black
                               _   -> green)
