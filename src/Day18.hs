{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day18 where

import           Control.DeepSeq             (NFData (..))
import           Control.Monad.State
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Char                   (isAlpha, isLower, isUpper,
                                              toLower, toUpper)
import           Data.Graph                  as G
import           Data.Graph.AStar
import qualified Data.HashSet                as HS
import           Data.List                   (isPrefixOf, isSuffixOf)
import           Data.List.Extra             (minimumOn, sortOn)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Debug.Trace

import           AoC
import           Search
import           Vis

type World = Map (Int,Int) Char

type Point = (Int,Int)

data BotState = BotState {
  world    :: World,
  botPos   :: Point,
  botKeys  :: Set Char,
  botGoals :: Map Char (Int,Int),
  keySeq   :: [Char],
  botPaths :: [[Point]]
  } deriving (Show)

instance NFData BotState  where
  rnf BotState{..} = botPaths `seq` ()


getInput :: FilePath -> IO World
getInput fn = parseGrid id <$> readFile fn -- "input/day18"

displayMap :: World -> String
displayMap m = drawString m (\p -> Map.findWithDefault ' ' p m)

entrance :: World -> (Int,Int)
entrance = fst . head . filter ((== '@') . snd) . Map.toList

keys :: World -> World
keys = Map.filter isLower

doors :: World -> World
doors = Map.filter isUpper

remaining :: World -> World
remaining = Map.filter isAlpha

flipMap :: Ord b => Map a b -> Map b a
flipMap = Map.foldrWithKey (\k x -> Map.insert x k) mempty

around :: Point -> [Point]
around (x,y) = [(x,y-1), (x-1,y), (x+1,y), (x,y+1)]

neighbors :: Point -> State BotState (HS.HashSet Point)
neighbors c = do
  m <- gets world
  ks <- gets botKeys
  pure $ neighbors' m ks c

neighbors' :: World -> Set Char -> Point -> HS.HashSet Point
neighbors' m ks c = do
  HS.fromList [(x,y) | (x,y) <- around c, reachableWithKeys ks (Map.lookup (x,y) m)]

-- Destination has a set of blockers and what we get when we get through them.
type Dest = (Point, Set Char, Char, [Point])

type KeysGraph = Map Point (Map Char Dest)

breakfast :: World -> [[Point]]
breakfast m = go (Set.fromList . Map.elems $ keys m) mempty (entrance m) []
  where
    km = k2k m
    pathlen = length . concat
    go need have pos ans
      -- | trace ("from " <> show pos <> "(" <> show (m Map.! pos) <> ") need " <> Set.toList need <> " have " <> Set.toList have) False = undefined
      | null need = ans
      | otherwise = (minimumOn pathlen $ parMap rdeepseq (descend need have) (possible pos have km)) <> ans

    descend need have (p, _, c, path) = go (Set.delete c need) (Set.insert c have) p [path]

{-
withSort :: World -> [[Point]]
withSort m = undefined
  where km = k2k m
        klocs = flipMap $ keys m
        dlocs = flipMap $ toLower <$> doors m
        kgr = k2kOn isLower m
        dgr = k2kOn isUpper m
        doorDeps d =
        -- g = G.graphFromEdges . map (\(Dep ts (Thing _ k)) -> (k,k, thingName <$> ts)) $ lkm
-}

graphviz :: World -> String
graphviz w = "digraph g {\n" <> unlines [each k | k <- Map.toList km] <> "\n}\n"
  where km = k2k w
        each :: (Point, Map Char Dest) -> String
        each (p, m) = unlines [ dst k | k <- Map.elems m ]
          where t = w Map.! p
                dst (_, drs, c, dsts)
                  | null drs = t : " -> " <> [c]
                  | otherwise = unlines [ toUpper dr : " -> " <> [c] | dr <- Set.toList drs]

possible :: Point -> Set Char -> KeysGraph -> [Dest]
possible p ks = filter (\x -> reachable x && needed x) . Map.elems . (Map.! p)
  where reachable (_,ds,_,_) = ds `Set.isSubsetOf` ks
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
k2kOn pred m = Map.fromList . fmap extractValue . parMap rdeepseq oneKey . (entrance m:) . Map.keys . keys $ m
  where
    oneKey kp = (kp, bfsOn (\(a,_,_,_) -> a) nf (kp, mempty, m Map.! kp, []))

    -- Get just the interesting destination paths.
    extractValue :: (Point, [(Point, Set Char, Char, [Point])]) -> (Point, Map Char Dest)
    extractValue (p, dsts) = (p, Map.fromListWith best . fmap byChar . filter justKeys $ dsts)
      where
        -- Keys, but not self (i.e., anything with a destination)
        justKeys (_,_,c,path) = pred c && (not . null) path

        pathsOf (_,_,_,p) = p

        -- Keep only the shorter ones
        best a@(_,_,_,ap) b@(_,_,_,bp) = if length ap > length bp then b else a
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

type Cache = Map World BotState

brute :: World -> (Int, BotState)
brute w = evalState (go (BotState w (entrance w) mempty (flipMap $ keys w) mempty mempty)) mempty
  where
    go :: BotState -> State Cache (Int, BotState)
    go b = get >>= \cache -> do
      b' <- proceed (allReachable b (botPos b)) b (Map.lookup (world b) cache)
      pure (length . concat $ botPaths b', b')

    pathlen = length . concat . botPaths

    proceed :: [Point] -> BotState -> Maybe BotState -> State Cache BotState
    proceed _ _ (Just b) = pure b
    proceed [] b _ = pure b
    proceed rs b _ = do
      cands <- traverse (each b) rs
      -- traceM ("reachable " <> show (map snd rs) <> " @ " <> show (pathlen <$> cands))
      let r = minimumOn pathlen $ cands
      modify (Map.insert (world b) r)
      pure r

    each :: BotState -> Point -> State Cache BotState
    each b@BotState{..} p = do
      let (Just path) = aStar (neighbors' world botKeys) (const . const $ (1::Int)) (const 0) (== p) botPos
          kl = filter isLower . map (world Map.!) $ path
          ks = Set.union botKeys . Set.fromList $ kl
          gs = Map.withoutKeys botGoals ks
          nw = foldr (\p' -> Map.insert p' '.') (Map.insert botPos '.' world) path
      let b' = b{world=Map.insert p '@' nw,
                 botPos=p,
                 botGoals=gs,
                 botPaths=path:botPaths,
                 keySeq=keySeq<>kl,
                 botKeys=ks}
      snd <$> go b'

reachableWithKeys :: Set Char -> Maybe Char -> Bool
reachableWithKeys _ Nothing = False
reachableWithKeys _ (Just '.') = True
reachableWithKeys _ (Just '#') = False
reachableWithKeys ks (Just x)
  | isLower x = True
  | isUpper x && Set.member (toLower x) ks = True
  | otherwise = False

allReachable :: BotState -> Point -> [Point]
allReachable b point = interesting $ bfsOn botPos nf b
  where
    obk = botKeys b
    interesting = filter (isLower . ((world b) Map.!)) . map botPos
    nf b@BotState{..} = map (moveFrom b) . filter (\p -> reachableWithKeys obk $ Map.lookup p world) $ around botPos
    moveFrom b@BotState{..} np =
      let nc = world Map.! np
          kl = case () of _
                            | isLower nc -> [nc]
                            | otherwise -> []
          ks = Set.union botKeys . Set.fromList $ kl
          gs = Map.withoutKeys botGoals ks
          nw = foldr (\p' -> Map.insert p' '.') (Map.insert botPos '.' world) [np]
      in b{world=Map.insert np '@' nw,
           botPos=np,
           botGoals=gs,
           botPaths=[np]:botPaths,
           keySeq=keySeq<>kl,
           botKeys=ks}


allReachable' :: BotState -> Point -> [Point]
allReachable' BotState{..} point = go 1 start botKeys world (nf world start)

  where
    start = ns botKeys [point] world
    nf m np = filter (isLower . (m Map.!)) $ np
    go :: Int -> [Point] -> Set Char -> World -> [Point] -> [Point]
    go n points ks m found
      | null points = found
      | otherwise = go (n+1) np ks up (nf m np <> found)
      where
        np = ns ks points up
        up = Map.union (Map.fromList [(p,'#') | p <- points]) m

    dd = HS.toList . HS.fromList

    ns ks points m = dd $ filter (\p -> reachableWithKeys ks $ Map.lookup p m) $ concatMap around points

do1 :: FilePath -> IO Int
do1 fp = sum . fmap length . breakfast <$> getInput fp

part1 :: IO Int
part1 = do1 "input/day18"

part2 :: IO Int
part2 = pure 0
