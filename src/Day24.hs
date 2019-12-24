{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day24 where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           AoC
import           Search
import           TwoD
import           Vis

type World = Map Point Char

getInput :: FilePath -> IO World
getInput fp = parseGrid id <$> readFile fp

adjacency :: World -> Point -> Int
adjacency m = length . filter (== '#') . map (\p' -> Map.findWithDefault '.' p' m) . around

bugs :: World -> World
bugs = Map.filter (== '#')

spaces :: World -> World
spaces = Map.filter (== '.')

automata :: World -> World
automata m = Map.union frombugs fromspaces
  where
    adjCounts = Map.mapWithKey (\k _ -> adjacency m k) m
    frombugs = Map.fromList [(k, if adjCounts Map.! k == 1 then '#' else '.') | k <- Map.keys (bugs m)]
    fromspaces = Map.fromList [(k, if (adjCounts Map.! k) `elem` [1,2] then '#' else '.') | k <- Map.keys (spaces m)]

displayWorld :: World -> IO ()
displayWorld m = putStrLn $ drawString m (\p -> Map.findWithDefault ' ' p m)

part1 :: World -> Int
part1 m = let (_,_,m') = findCycle id (iterate automata m) in
            bioDiv m'
  where
    ((mnx,mny),(mxx,mxy)) = bounds2d m

    bmap = Map.fromList $ zip [(x,y) | y <- [mny..mxy], x <- [mnx..mxx]] [2^x | x <- [0..]]
    bioDiv :: World -> Int
    bioDiv = sum . map ((bmap Map.!) . fst) . filter ((== '#') . snd) . Map.toList

part2 :: World -> Int
part2 = const 0
