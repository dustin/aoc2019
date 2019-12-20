{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day18 where

import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Char                   (isLower, isUpper, toLower,
                                              toUpper)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromJust)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           AoC
import           Search
import           Vis

type World = Map (Int,Int) Char

type Point = (Int,Int)

getInput :: FilePath -> IO World
getInput fn = parseGrid id <$> readFile fn -- "input/day18"

displayMap :: World -> String
displayMap m = drawString m (\p -> Map.findWithDefault ' ' p m)

entrances :: World -> [(Int,Int)]
entrances = fmap fst . filter ((== '@') . snd) . Map.toList

entrance :: World -> (Int,Int)
entrance = head . entrances

keys :: World -> World
keys = Map.filter isLower

flipMap :: Ord b => Map a b -> Map b a
flipMap = Map.foldrWithKey (\k x -> Map.insert x k) mempty

around :: Point -> [Point]
around (x,y) = [(x,y-1), (x-1,y), (x+1,y), (x,y+1)]

-- Destination has a set of blockers and what we get when we get through them.
type Dest = (Point, Set Char, Char, [Point])

type KeysGraph = Map Point (Map Char Dest)

type DState = (Set Point, Set Char, Set Char)

dijk :: World -> Maybe (Int, [DState])
dijk m = dijkstra nf start isDone
  where
    start = (Set.fromList $ entrances m, (Set.fromList . Map.elems $ keys m), mempty)
    km = k2k m
    isDone (_,x,_) = null x
    nf (ps, w, h) = map (\(op,(np,_,c,ps')) -> (length ps', (adjps op np ps, Set.delete c w, Set.insert c h))) $
                    concatMap (\p -> zip (repeat p) (possible p h km)) ps
      where adjps op np = Set.insert np . Set.delete op

graphviz :: World -> String
graphviz w = "digraph g {\n" <> unlines [each k | k <- Map.toList km] <> "\n}\n"
  where km = k2k w
        each :: (Point, Map Char Dest) -> String
        each (p, m) = unlines [ dst k | k <- Map.elems m ]
          where t = w Map.! p
                dst (_, drs, c, _)
                  | null drs = t : " -> " <> [c]
                  | otherwise = unlines [ toUpper dr : " -> " <> [c] | dr <- Set.toList drs]

possible :: Point -> Set Char -> KeysGraph -> [Dest]
possible p ks = filter (\x -> needed x && reachable x) . Map.elems . (Map.! p)
  where reachable (_,ds,_,_) = null ds || ds `Set.isSubsetOf` ks
        needed (_,_,c,_) = Set.notMember c ks

showk2k :: Map Point (Map Char Dest) -> IO ()
showk2k = mapM_ showk . Map.toList
  where
    showk :: (Point, Map Char Dest) -> IO ()
    showk (p, m) = putStrLn (show p) >> mapM_ showDest (Map.elems m)
    showDest (dp, ds, c, path) = putStrLn $ mconcat ["\t", show c, "@", show dp,
                                                     " needs: ", Set.toList ds,
                                                     " len: ", show (length path)]

k2kOn :: (Char -> Bool) -> World -> Map Point (Map Char Dest)
k2kOn pr m = Map.fromList . fmap extractValue . parMap rdeepseq oneKey . (entrances m<>) . Map.keys . keys $ m
  where
    oneKey kp = (kp, bfsOn (\(a,_,_,_) -> a) nf (kp, mempty, m Map.! kp, []))

    -- Get just the interesting destination paths.
    extractValue :: (Point, [(Point, Set Char, Char, [Point])]) -> (Point, Map Char Dest)
    extractValue (p, dsts) = (p, Map.fromListWith best . fmap byChar . filter justKeys $ dsts)
      where
        -- Keys, but not self (i.e., anything with a destination)
        justKeys (_,_,c,path) = pr c && (not . null) path

        -- Keep only the shorter ones
        best a@(_,_,_,apt) b@(_,_,_,bp) = if length apt > length bp then b else a
        byChar d@(_, _, c, _) = (c, d)

    nf (p, ks, _, path) = foldMap each $ around p
      where each np = let c = (m Map.! np) in
                        case () of _
                                     | isUpper c -> [(np, Set.insert (toLower c) ks, c, np:path)]
                                     | c == '#' -> []
                                     | otherwise -> [(np, ks, c, np:path)]

-- Map from every key to every other key
k2k :: World -> Map Point (Map Char Dest)
k2k = k2kOn isLower

do1 :: FilePath -> IO Int
do1 fp = fst . fromJust . dijk <$> getInput fp

do2 :: FilePath -> IO Int
do2 fp = fst . fromJust . dijk <$> getInput fp

p2Input :: FilePath -> IO World
p2Input fp = do
  orig <- getInput fp
  let (x,y) = entrance orig
  pure $ Map.unions [
    Map.fromList [((x+xo, y+yo), '@') | xo <- [-1,1], yo <- [-1,1]],
    Map.fromList [((x+xo, y+yo), '#') | xo <- [-1..1], yo <- [-1..1]],
    orig]

part1 :: IO Int
part1 = do1 "input/day18"

part2 :: IO Int
part2 = fst . fromJust . dijk <$> p2Input "input/day18"
