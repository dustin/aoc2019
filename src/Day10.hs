{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day10 where

import           Control.Parallel.Strategies (parList, parMap, rseq, using)
import           Data.List                   (intercalate, sortOn)
import           Data.List.Extra             (maximumOn)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map

import           Advent.AoC
import           Advent.TwoD
import           Advent.Vis

newtype World = World (Map Point Char) deriving (Show)

instance Bounded2D World where
  bounds2d (World m) = bounds2d m

display :: World -> String
display (World m) = drawString m (mapCharFun m id)

getInput :: FilePath -> IO World
getInput fn = parseInput <$> readFile fn

parseInput :: String -> World
parseInput = World . parseGrid id

allSteroids :: World -> [Point]
allSteroids (World wm) = Map.keys . Map.filter (== '#') $ wm

line :: Point -> Point -> [Point]
line (x1,y1) (x2,y2)
  | dx == 0 = [(x1,y) | y <- [min y1 y2 .. max y1 y2]]
  | dy == 0 = [(x,y1) | x <- [min x1 x2 .. max x1 x2]]
  | otherwise = [(x1 + (i*stepx), y1 + (i*stepy)) | i <- [1.. g - 1]]

  where dx = x2 - x1
        dy = y2 - y1
        g = gcd dx dy
        stepx = dx `div` g
        stepy = dy `div` g

sees :: World -> Point -> [Point]
sees w p = flt [] aa
  where
    aa = sortOn (mdist2 p) (allSteroids w) `using` parList rseq

    flt l [] = l
    flt ks (x:xs)
      | x == p = flt ks xs
      | obscuredBy ks x = flt ks xs
      | otherwise = flt (x:ks) xs

    obscuredBy ks me = any f ks
      where
        f k = k `elem` line p me

dbgSees :: World -> Point -> String
dbgSees w@(World wm) p@(px,py) = d <> "\n" <> pairs
  where
    seize = sees w p
    d = display . World $ Map.union (Map.fromList $ map (,'X') seize) wm
    pairs = intercalate "\n" (zipWith (\x y -> show x <> " -> " <> show y) seize (map (\(x,y) -> (x-px, y-py)) seize))

best :: World -> (Point,Int)
best w = maximumOn snd cz
  where aa = allSteroids w
        cz = zip aa (length . sees w <$> aa)

bestPar :: World -> (Point,Int)
bestPar w = maximumOn snd cz
  where aa = allSteroids w
        cz = zip aa (parMap rseq (length . sees w) aa)

part1 :: IO (Point,Int)
part1 = best <$> getInput "input/day10"

sweep :: World -> Point -> [Point]
sweep w o = sortOn (θ o) . sees w $ o
  where θ (x1,y1) (x2,y2) = -atan2 (x2 -. x1) (y2 -. y1) + pi
        (-.) :: Int -> Int -> Double
        a -. b = fromIntegral (a - b)

part2 :: IO Int
part2 = getInput "input/day10" >>= \w -> pure . ans . (!! 199) $ sweep w (fst (best w))
  where ans (x,y) = x * 100 + y
