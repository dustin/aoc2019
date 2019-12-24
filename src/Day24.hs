{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day24 where

import           Data.Foldable   (foldMap)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           AoC
import           Search
import           TwoD
import           Vis

type World = Set Point

getInput :: FilePath -> IO World
getInput fp = Map.keysSet . Map.filter (== '#') . parseGrid id <$> readFile fp

-- Number of nearby bugs
adjacency :: Ord k => (k -> [k]) -> Set k -> k -> Int
adjacency arnd m = length . filter (`Set.member` m) . arnd

spaces :: Ord k => (k -> [k]) -> Set k -> Set k
spaces arnd m = (`Set.difference` m) . foldMap (Set.fromList . arnd) $ m

automata :: Ord k => (k -> [k]) -> Set k -> Set k
automata arnd m = Set.union frombugs fromspaces
  where
    adjc k = adjacency arnd m k
    frombugs = Set.filter (\k -> adjc k == 1) $ m
    fromspaces = Set.filter (\k -> adjc k `elem` [1,2]) . spaces arnd $ m

displayWorld :: World -> IO ()
displayWorld s = putStrLn $ drawString m (\p -> Map.findWithDefault '.' p m)
  where m = Map.fromList [(k,'#') | k <- Set.toList s]

part1 :: World -> Int
part1 m = let (_,_,m') = findCycle id (iterate (automata arnd) m) in
            bioDiv m'
  where
    arnd = filter (\(x,y) -> x `elem` [0..4] && y `elem` [0..4]) . around

    bmap = Map.fromList $ zip [(x,y) | y <- [0..4], x <- [0..4]] [2^x | x <- [0::Int ..]]
    bioDiv = sum . map ((bmap Map.!)) . Set.toList

type Point3 = (Int,Int,Int)
type World3 = Set Point3

upgrade :: World -> World3
upgrade = Set.map (\(x,y) -> (x,y,0))

up :: Point3 -> [Point3]
up (_,0,z) = [(2,1,z-1)]
up (2,3,z) = [(x,4,z+1) | x <- [0..4]]
up (x,y,z) = [(x,y-1,z)]

left :: Point3 -> [Point3]
left (0,_,z) = [(1,2,z-1)]
left (3,2,z) = [(4,y,z+1) | y <- [0..4]]
left (x,y,z) = [(x-1,y,z)]

right :: Point3 -> [Point3]
right (4,_,z) = [(3,2,z-1)]
right (1,2,z) = [(0,y,z+1) | y <- [0..4]]
right (x,y,z) = [(x+1,y,z)]

down :: Point3 -> [Point3]
down (_,4,z) = [(2,3,z-1)]
down (2,1,z) = [(x,0,z+1) | x <- [0..4]]
down (x,y,z) = [(x,y+1,z)]

around3 :: Point3 -> [Point3]
around3 p = mconcat [up p, left p, right p, down p]

depths :: World3 -> Set Int
depths = Set.map thrd

displayWorld3 :: World3 -> IO ()
displayWorld3 m3 = mapM_ (\l -> putStrLn ("Level " <> show l) >> displayWorld (downgrade l)) $ depths m3
  where downgrade l = Set.map (\(x,y,_) -> (x,y)) . Set.filter ((== l) . thrd) $ m3

part2 :: Int -> World -> Int
part2 mins = part2' mins . upgrade

part2' :: Int -> World3 -> Int
part2' mins m3 = length $ (iterate (automata around3) m3 !! mins)
