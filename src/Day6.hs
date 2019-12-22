{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day6 where

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (listToMaybe)
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

parseAll :: Parser OMap
parseAll = omap <$> parseOrbit `endBy` "\n"
  where omap = Map.fromListWith (const $ error "woops") . fmap swap

getInput :: IO OMap
getInput = parseFile parseAll "input/day6"

countOrbits :: OMap -> Int
countOrbits omap = sum orbits
  where orbits = lu <$> omap
        lu v = maybe 1 succ (Map.lookup v orbits)

-- the subtract 3 in there is to remove the endpoints and the first
-- part of the transfer.  e.g. listToMaybe returns Just
-- ["SAN","I","D","E","J","K","YOU"] for the small example.  It's
-- considered four transfers from K to J to E to D to I according to
-- the example.
countTransfers :: Text -> Text -> Map Text Text -> Maybe Int
countTransfers f t omap = fmap (subtract 3 . length) . listToMaybe . filter ((== t) . head) $ bfsOn head nf [f]
  where
    nf []       = []
    nf xs@(x:_) = (:xs) <$> dmap Map.! x
    dmap = Map.unionWith (<>) (fmap (:[]) omap) rmap
    rmap = Map.fromListWith (<>) . fmap (fmap (:[]) . swap) $ Map.toList omap

part1 :: IO Int
part1 = countOrbits <$> getInput

part2 :: IO (Maybe Int)
part2 = countTransfers "YOU" "SAN" <$> getInput
