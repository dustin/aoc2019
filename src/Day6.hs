{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day6 where

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text, pack)
import           Data.Tuple           (swap)
import           Text.Megaparsec      (endBy, many, sepBy)
import           Text.Megaparsec.Char (alphaNumChar)


import           AoC
import           Search

type Orbit = (Text, Text)

type OMap = Map Text Text

parseOrbit :: Parser Orbit
parseOrbit = do
  [ob1, ob2] <- object `sepBy` ")"
  pure (ob1, ob2)
  where
    object :: Parser Text
    object = pack <$> many alphaNumChar

parseAll :: Parser [Orbit]
parseAll = parseOrbit `endBy` "\n"

getInput :: IO [Orbit]
getInput = parseFile parseAll "input/day6"

mapIt :: [Orbit] -> OMap
mapIt = Map.fromListWith (const $ error "woops") . fmap swap

countOrbits :: [Orbit] -> Int
countOrbits o = sum orbits
  where omap = mapIt o
        orbits = lu <$> omap
        lu v = maybe 1 succ (Map.lookup v orbits)

-- the subtract 2 in there is to remove the endpoints
countTransfers :: Text -> Text -> [Orbit] -> Maybe Int
countTransfers f t o = subtract 2 . fst <$> dijkstra (dmap Map.!) f t
  where
    omap = mapIt o
    dmap = fmap (1,) <$> Map.unionWith (<>) (fmap (:[]) omap) rmap
    rmap = Map.fromListWith (<>) . fmap (fmap (:[])) $ o

part1 :: IO Int
part1 = countOrbits <$> getInput

part2 :: IO (Maybe Int)
part2 = countTransfers "YOU" "SAN" <$> getInput
