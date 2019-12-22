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
import           TwoD
import           Vis

type World = Map (Int,Int) Char

getInput :: FilePath -> IO World
getInput fn = parseGrid id <$> readFile fn -- "input/day20"

displayMap :: World -> String
displayMap m = drawString m (\p -> Map.findWithDefault ' ' p m)

portalRev :: Map Point PortalID -> Map PortalID [Point]
portalRev m = Map.fromListWith (<>) [(v,[k]) | (k,v) <- Map.toList m]

warpMap :: Map Point PortalID -> Map Point Point
warpMap = Map.fromList . concatMap m . Map.elems . portalRev
  where
    m [v1, v2] = [(v1,v2), (v2,v1)]
    m _        = []

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

insidePortals :: World -> Map Point PortalID
insidePortals m = Map.filterWithKey (\k _ -> inside k) . portals $ m
  where
    ((lowx,lowy),(hix,hiy)) = bounds2d . Map.filter (== '.') $ m
    outside (x,y) = x == lowx || x == hix || y == lowy || y == hiy
    inside = not . outside

findPath2 :: World -> Maybe Int
findPath2 m = fst <$> dijkstra nf start (== end)
  where
    ps = portals m
    pr = portalRev ps
    insideMap = insidePortals m
    inside = (`Map.member` insideMap)
    outside = not . (`Map.member` insideMap)

    warps = warpMap ps
    start :: (Int, Point)
    start = (0, head $ pr Map.! ('A', 'A'))
    end = (0, head $ pr Map.! ('Z', 'Z'))
    nf :: (Int,Point) -> [(Int,(Int,Point))]
    nf p = neighbors p <> warp p
    neighbors (l,p) = fmap (\p' -> (1,(l,p'))) $ filter (\p' -> Map.lookup p' m == Just '.') (around p)
    warp (l,p)
      | l == 0 && outside p = []
      | otherwise = case Map.lookup p warps of
                      Nothing -> []
                      Just p' -> [(1, (if inside p then l + 1 else l - 1, p'))]

part1 :: IO Int
part1 = fst . fromJust . findPath <$> getInput "input/day20"

part2 :: IO Int
part2 = fromJust . findPath2 <$> getInput "input/day20"
