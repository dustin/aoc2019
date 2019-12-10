{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day10 where

import           Data.List       (intercalate, sortOn)
import           Data.List.Extra (maximumOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           AoC
import           Vis

newtype World = World (Map (Int,Int) Char) deriving (Show)

instance Bounded2D World where
  bounds2d (World m) = bounds2d m

display :: World -> String
display (World m) = drawString m (mapCharFun m id)

getInput :: FilePath -> IO World
getInput fn = parseInput . lines <$> readFile fn

allSteroids :: World -> [(Int,Int)]
allSteroids (World wm) = Map.keys . Map.filter (== '#') $ wm

lineTo :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
lineTo p1@(x1,y1) p2@(x2,y2)
  | dx == 0 = [(x1,y) | y <- [min y1 y2 .. max y1 y2]]
  | dy == 0 = [(x,y1) | x <- [min x1 x2 .. max x1 x2]]
  | otherwise = [(x1 + (i*stepx), y1 + (i*stepy)) | i <- [1.. g - 1]]

  where dx = x2 - x1
        dy = y2 - y1
        g = gcd dx dy
        stepx = dx `div` g
        stepy = dy `div` g


sees :: World -> (Int,Int) -> [(Int,Int)]
sees w@(World wm) p@(px,py) = flt [] aa
  where
    aa = sortOn (mdist2 p) $ allSteroids w

    flt l [] = l
    flt ks (x:xs)
      | x == p = flt ks xs
      | obscuredBy ks x = flt ks xs
      | otherwise = flt (x:ks) xs

    obscuredBy ks me = any f ks
      where
        f k = k `elem` lineTo p me

dbgSees :: World -> (Int,Int) -> String
dbgSees w@(World wm) p@(px,py) = d <> "\n" <> pairs
  where
    seize = sees w p
    d = display . World $ Map.union (Map.fromList $ map (,'X') $ seize) wm
    pairs = intercalate "\n" $ (zipWith (\x y -> show x <> " -> " <> show y) seize $ map (\(x,y) -> (x-px, y-py)) $ seize)

best :: World -> ((Int,Int),Int)
best w = maximumOn snd cz
  where aa = allSteroids w
        cz = zip aa (length . sees w <$> aa)

parseInput :: [String] -> World
parseInput lns = World $ Map.fromList $ concatMap (\(y,r) -> map (\(x,c) -> ((x,y),p c)) $ zip [0..] r) $ zip [0..] lns
  where p = id

part1 :: IO ((Int,Int),Int)
part1 = best <$> getInput "input/day10"

part2 :: IO Int
part2 = pure 0
