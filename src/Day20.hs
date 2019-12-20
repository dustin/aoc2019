{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day20 where

import           Data.Char       (isAlpha)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust, mapMaybe, maybeToList)

import           AoC
import           Search
import           Vis

type World = Map (Int,Int) Char

type Point = (Int,Int)

getInput :: FilePath -> IO World
getInput fn = parseGrid id <$> readFile fn -- "input/day20"

displayMap :: World -> String
displayMap m = drawString m (\p -> Map.findWithDefault ' ' p m)

around :: Point -> [Point]
around (x,y) = [(x,y-1), (x-1,y), (x+1,y), (x,y+1)]

flipMap :: Ord b => Map a b -> Map b a
flipMap = Map.foldrWithKey (\k x -> Map.insert x k) mempty

portalRev :: Map Point PortalID -> Map PortalID [Point]
portalRev m = Map.fromListWith (<>) [(v,[k]) | (k,v) <- Map.toList m]

warpMap :: Map Point PortalID -> Map Point Point
warpMap = Map.fromList . concatMap m . Map.toList . portalRev
  where
    m (_,[v1, v2]) = [(v1,v2), (v2,v1)]
    m _            = []

type PortalID = (Char, Char)

portals :: World -> Map Point PortalID
portals m = Map.fromList $ mapMaybe (lu updown) chars <> mapMaybe (lu leftright) chars
  where
    chars = Map.toList $ Map.filter isAlpha m
    lu f (p,_) = isPortal (f p, traverse (`Map.lookup` m) (f p))
    leftright (x,y) = [(x+xo, y) | xo <- [-1..1]]
    updown (x,y) = [(x, y+yo) | yo <- [-1..1]]

    isPortal ([pa,_,pc], Just [a,b,c]) =
      if isAlpha b && (a == '.' || c == '.') && (isAlpha a || isAlpha c)
      then Just (if a == '.' then (pa, (b,c)) else (pc, (a,b)))
      else Nothing
    isPortal _ = Nothing

findPath :: World -> Maybe (Int, [Point])
findPath m = dijkstra nf start (== end)
  where
    ps = portals m
    pr = portalRev ps
    warps = warpMap ps
    start = head $ pr Map.! ('A', 'A')
    end = head $ pr Map.! ('Z', 'Z')
    nf p = fmap (1,) (filter (\p' -> Map.lookup p' m == Just '.') (around p) <> maybeToList (Map.lookup p warps))

part1 :: IO Int
part1 = fst . fromJust . findPath <$> getInput "input/day20"

part2 :: IO Int
part2 = pure 0
