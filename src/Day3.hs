{-# LANGUAGE TupleSections #-}

module Day3 where

import           Control.Applicative        (liftA2, (<|>))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Text.Megaparsec            (endBy, sepBy)
import           Text.Megaparsec.Char.Lexer (decimal)

import           AoC                        (Parser, mdist2, parseFile)


data Dir = R | D | U | L deriving(Show)

type Wire = [(Dir, Int)]

parseWire :: Parser Wire
parseWire = parseSeg `sepBy` ","
  where
    parseSeg = liftA2 (,) parseDir decimal
    parseDir = R <$ "R" <|> D <$ "D" <|> U <$ "U" <|> L <$ "L"

parseAll :: Parser [Wire]
parseAll = parseWire `endBy` "\n"

getInput :: IO [Wire]
getInput = parseFile parseAll "input/day3"

path :: Wire -> [((Int,Int),Int)]
path w = go (0,0) 1 [] w
  where go _ _ l [] = l
        go p n l ((_,0):xs) = go p n l xs
        go (x,y) n l ((d,dist):xs) = go (npos d) (n+1) ((npos d,n):l) ((d,dist-1):xs)
          where npos R = (x+1, y)
                npos L = (x-1, y)
                npos U = (x,   y+1)
                npos D = (x,   y-1)

wireMap :: Wire -> Map (Int,Int) Int
wireMap = Map.fromList . path

overlaps :: [Map (Int,Int) Int] -> Map (Int,Int) Int
overlaps = Map.unionsWith (+) . (fmap.fmap) (const 1)

part1 :: [Wire] -> Int
part1 wires = let maps = overlaps $ wireMap <$> wires
                  ols = map (mdist2 (0,0) . fst) . filter ((> 1) . snd) $ Map.toList maps
              in minimum ols

part2 :: [Wire] -> Int
part2 wires = let maps = wireMap <$> wires
                  olaps = Map.filter (> 1) . overlaps $ maps
                  dists = Map.mapWithKey (\k _ -> sum . fmap (Map.! k) $ maps) $ olaps
              in minimum dists
