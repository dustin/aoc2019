{-# LANGUAGE TupleSections #-}

module Day3 where

import           Control.Applicative        (liftA2, (<|>))
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Text.Megaparsec            (endBy, sepBy)
import           Text.Megaparsec.Char.Lexer (decimal)

import           AoC                        (Parser, mdist2, parseFile)
import           Vis


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

path :: Wire -> [(Int,Int)]
path = scanl1 plus . expand
  where
    (a1, b1) `plus` (a2, b2) = (a1 + a2, b1 + b2)
    expand = concatMap (\(d,n) -> replicate n (dir d))
    dir U = ( 0,  1)
    dir D = ( 0, -1)
    dir R = ( 1,  0)
    dir L = (-1,  0)

wireMap :: Wire -> Map (Int,Int) Int
wireMap = Map.fromList . flip zip [1..] . path

overlaps :: [Map (Int,Int) a] -> [(Int,Int)]
overlaps = Map.keys . foldl1 Map.intersection

part1 :: [Wire] -> Int
part1 wires = let olaps = overlaps $ wireMap <$> wires
                  ols = mdist2 (0,0) <$> olaps
              in minimum ols

part2 :: [Wire] -> Int
part2 wires = let maps = wireMap <$> wires
                  olaps = overlaps maps
                  dists = (\k -> sum . fmap (Map.! k) $ maps) <$> olaps
              in minimum dists

drawInput :: FilePath -> IO ()
drawInput fp = do
  inp <- fmap wireMap <$> getInput
  let colored = zipWith (\m c -> fmap (const c) m) inp [green, blue]
      allps = Map.unionsWith (const . const $ red) colored
  draw fp allps (\p -> Map.findWithDefault white p allps)
