{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day14 where

import           Data.Char                  (isAlphaNum)
import qualified Data.Graph                 as G
import           Data.List                  (foldl')
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text, pack)
import           Text.Megaparsec            (endBy, satisfy, sepBy, some)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Advent.AoC
import           Advent.Search

data Thing = Thing {
  thingMakes :: Int,
  thingName  :: Text
  } deriving (Show)

data Dep = Dep [Thing] Thing deriving (Show)

parseThing :: Parser Thing
parseThing = Thing <$> decimal <* " " <*> word
  where word = pack <$> some (satisfy isAlphaNum)

parseDep :: Parser Dep
parseDep = Dep <$> (parseThing `sepBy` ", ") <*> (" => " *> parseThing)

getInput :: FilePath -> IO [Dep]
getInput = parseFile parseAll

parseAll :: Parser [Dep]
parseAll = parseDep `endBy` "\n"

oreReq :: [Dep] -> Int -> Int
oreReq deps req = made Map.! "ORE"
  where
    -- Map of items to their requirements (q, name)
    reqm = Map.fromList [(t, (n, (\(Thing n' t') -> (n', t')) <$> nds)) | (Dep nds (Thing n t)) <- deps]
    topSorted = let (g, v2n, _) = G.graphFromEdges . map (\(Dep ts (Thing _ k)) -> (k,k, thingName <$> ts)) $ deps
                in fst3 . v2n <$> G.topSort g
    made = foldl' f (Map.singleton "FUEL" req) topSorted
    f want next = Map.unionWith (+) (Map.delete next want) adds
      where
        (rq, reqs) = reqm Map.! next
        q = ((want Map.! next) + rq - 1) `div` rq
        adds = Map.fromList . map (\(x,t) -> (t,q*x)) $ reqs

part1 :: IO Int
part1 = (`oreReq` 1) <$> getInput "input/day14"

part2 :: IO Int
part2 = do
  inp <- getInput "input/day14"
  -- autoBinSearch returns the highest LT value, so we're off by one.
  pure $ autoBinSearch (\x -> compare (oreReq inp x) 1000000000000) - 1
