{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day12 where

import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.List                   (elemIndex, foldl')
import           Data.Maybe                  (fromJust)
import           Text.Megaparsec             (endBy)
import           Text.Megaparsec.Char        (space, string)
import           Text.Megaparsec.Char.Lexer  (decimal, signed)

import           AoC
import           Search

data Moon = Moon {
  position :: (Int, Int, Int),
  velocity :: (Int, Int, Int)
  } deriving (Show, Eq, Ord)

parsePos :: Parser Moon
parsePos = do
  _ <- string "<x="
  x <- sd
  _ <- string ", y="
  y <- sd
  _ <- string ", z="
  z <- sd
  _ <- string ">"
  pure $ Moon (x, y, z) (0, 0, 0)

  where sd = signed space decimal

parseAll :: Parser [Moon]
parseAll = parsePos `endBy` "\n"

getInput :: FilePath -> IO [Moon]
getInput = parseFile parseAll

gravitize1 :: Moon -> Moon -> Moon
gravitize1 (Moon p@(x1,y1,z1) (vx,vy,vz)) (Moon (x2,y2,z2) _) =
  Moon p (vx + delta x1 x2, vy + delta y1 y2, vz + delta z1 z2)
  where delta a b
          | a == b = 0
          | a < b = 1
          | otherwise = -1

--  It's OK to test a moon against its own position since that's 0
-- (previously, I was doing a select to pull out others)
gravitize :: [Moon] -> [Moon]
gravitize moons = (\m -> foldl' gravitize1 m moons) <$> moons

move :: [Moon] -> [Moon]
move = map move1
  where
    move1 (Moon (x,y,z) v@(vx,vy,vz)) = Moon (x+vx, y+vy, z+vz) v

step :: [Moon] -> [Moon]
step = move . gravitize

energy :: [Moon] -> Int
energy = sum . map te
  where te (Moon p v) = e p * e v
        e (a,b,c) = abs a + abs b + abs c

-- This was my original implementation.  It was kind of slow, but also
-- kind of dumb.
posCycles' :: [[Moon]] -> [Int]
posCycles' ls = parMap rdeepseq axis [fst3, snd3, thrd]
  where
    poses :: [([Int], [Int], [Int])]
    poses = tr <$> fmap position <$> ls

    axis = fc . (<$> poses)

    tr :: [(Int, Int, Int)] -> ([Int], [Int], [Int])
    tr = foldr (\(x,y,z) (xs,ys,zs) -> (x:xs, y:ys, z:zs)) ([], [], [])

    -- I tried looking for just the first position, but that can
    -- repeat more frequently.  Two positions in a row is less likely.
    fc :: [[Int]] -> Int
    fc l = snd3 . findCycle id $ zip l $ tail l

type Four = ((Int,Int), (Int,Int), (Int,Int), (Int,Int))

posCycles :: [[Moon]] -> [Int]
posCycles ls = parMap rdeepseq axis [fst3, snd3, thrd]
  where
    maxes :: Moon -> ((Int,Int), (Int,Int), (Int,Int))
    maxes (Moon (x,y,z) (vx,vy,vz)) = ((x,vx), (y,vy), (z,vz))

    axes :: [Moon] -> ([(Int,Int)], [(Int,Int)], [(Int,Int)])
    axes moon = (fst3 <$> m, snd3 <$> m, thrd <$> m)
      where m = maxes <$> moon

    poses :: [([(Int,Int)], [(Int,Int)], [(Int,Int)])]
    poses = axes <$> ls

    axis = fc . (<$> poses)

    fc (x:xs) = succ . fromJust $ elemIndex x xs

part1 :: IO Int
part1 = do
  moons <- getInput "input/day12"
  let steps = iterate step moons
  pure . energy $ (steps !! 1000)

part2On :: FilePath -> IO Int
part2On fn = do
  moons <- getInput fn
  let steps = iterate step moons
      cycs = posCycles steps
  pure $ foldr lcm 1 cycs

part2 :: IO Int
part2 = part2On "input/day12"
