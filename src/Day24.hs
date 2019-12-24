{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day24 where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           AoC
import           Search
import           TwoD
import           Vis

type World = Map Point Char

getInput :: FilePath -> IO World
getInput fp = parseGrid id <$> readFile fp

adjacency :: Ord k => (k -> [k]) -> Map k Char -> k -> Int
adjacency arnd m = length . filter (== '#') . map (\p' -> Map.findWithDefault '.' p' m) . arnd

bugs :: Map k Char -> [k]
bugs = Map.keys . Map.filter (== '#')

spaces :: Ord k => (k -> [k]) -> Map k Char -> [k]
spaces arnd m = Set.toList . Set.fromList . filter (\p -> Map.lookup p m /= Just '#') . concatMap arnd . bugs $ m

automata :: Ord k => (k -> [k]) -> Map k Char -> Map k Char
automata arnd m = Map.unions [frombugs, fromspaces, m]
  where
    adjc k = adjacency arnd m k
    frombugs = Map.fromList [(k, if adjc k == 1 then '#' else '.') | k <- bugs m]
    fromspaces = Map.fromList [(k, '#')| k <- spaces arnd m, shouldSpace k]
    shouldSpace k = Map.lookup k m == Just '.' && adjc k `elem` [1,2]

automata3 :: Ord k => (k -> [k]) -> Map k Char -> Map k Char
automata3 arnd m = Map.union frombugs fromspaces
  where
    adjc k = adjacency arnd m k
    frombugs = Map.fromList [(k, if adjc k == 1 then '#' else '.') | k <- bugs m]
    fromspaces = Map.fromList [(k, '#')| k <- spaces arnd m, shouldSpace k]
    shouldSpace k = Map.lookup k m /= Just '#' && adjc k `elem` [1,2]

displayWorld :: World -> IO ()
displayWorld m = putStrLn $ drawString m (\p -> Map.findWithDefault '.' p m)

part1 :: World -> Int
part1 m = let (_,_,m') = findCycle id (iterate (automata around) m) in
            bioDiv m'
  where
    ((mnx,mny),(mxx,mxy)) = bounds2d m

    bmap = Map.fromList $ zip [(x,y) | y <- [mny..mxy], x <- [mnx..mxx]] [2^x | x <- [0..]]
    bioDiv = sum . map ((bmap Map.!) . fst) . filter ((== '#') . snd) . Map.toList

type Point3 = (Int,Int,Int)
type World3 = Map Point3 Char

upgrade :: World -> World3
upgrade = Map.fromList . map (\((x,y),c) -> ((x,y,0),c)) . Map.toList

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

depths :: World3 -> [Int]
depths = Set.toList . Set.fromList . map thrd . Map.keys

displayWorld3 :: World3 -> IO ()
displayWorld3 m3 = mapM_ (\l -> putStrLn ("Level " <> show l) >> displayWorld (downgrade l)) $ depths m3
  where downgrade l = Map.fromList . map (\((x,y,_),c) -> ((x,y),c)) . filter ((== l) . thrd . fst) . Map.toList $ m3

part2 :: Int -> World -> Int
part2 mins m = let m3 = upgrade m
                   gens = iterate (automata3 around3) m3
                   fstate = gens !! mins
               in length . bugs $ fstate
