{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day21 where

import           Data.Bits  (shiftL, shiftR, testBit, (.&.), (.|.))
import           Data.Char  (chr, ord)
import           Data.Word  (Word8)

import           ComputerST (FinalState (..), Instructions, executeWithinIns,
                             readInstructions)

import           Search

getInput :: IO Instructions
getInput = readInstructions "input/day21"

runOne :: String -> Instructions -> Either String Int
runOne ins prog = case executeWithinIns 1000000 (fmap ord ins) prog of
                    Right (FinalState{outputs}) -> parse outputs
                    Left x -> error ("Unexpected termination: " <> show x)
  where parse oot
          | all (< 128) oot = Left (fmap chr oot)
          | otherwise = Right (last oot)

doRun :: String -> IO ()
doRun s = do
  prog <- getInput
  case runOne s prog of
    Right x -> print x
    Left e  -> putStr e

data Op = AND Reg Reg | OR Reg Reg | NOT Reg Reg deriving (Show, Eq, Ord)

data Reg = A | B | C | D | E | F | G | H | T | J deriving (Show, Enum, Bounded, Eq, Ord)

opToWord :: Op -> Word8
opToWord o = (opo o `shiftL` 6) .|. (fromIntegral (fromEnum r) `shiftL` 1) .|. ww w
  where
    opo (AND _ _) = 1
    opo (OR  _ _) = 2
    opo (NOT _ _) = 3

    (r,w) = case o of
              AND r' w' -> (r',w')
              OR  r' w' -> (r',w')
              NOT r' w' -> (r',w')

    ww J = 1
    ww _ = 0

opFromWord :: Word8 -> Op
opFromWord i = c r w
  where
    c = case i `shiftR` 6 of
          1 -> AND
          2 -> OR
          3 -> NOT
    r = toEnum . fromEnum $ (i `shiftR` 1) .&. 0xf
    w = if testBit i 0 then J else T

-- Minimal complete instance: range, index and inRange.

type Script = [Op]

type Terrain = [Bool]

toTerrain :: String -> Terrain
toTerrain = map (== '#')

shouldJump4 :: [Terrain]
shouldJump4 = toTerrain <$> [".###", "#.##", "##.#"]

shouldNotJump4 :: [Terrain]
shouldNotJump4 = toTerrain <$> ["####", "#..."]

toScript :: [Op] -> String
toScript = unlines . map show

evaluate :: Script -> Terrain -> Bool
evaluate sins sensors = eval sins False False
  where
    eval [] _ j             = j
    eval ((AND r J):xs) t j = eval xs t (j && rd r t j)
    eval ((OR  r J):xs) t j = eval xs t (j || rd r t j)
    eval ((NOT r J):xs) t j = eval xs t (not (rd r t j))
    eval ((AND r T):xs) t j = eval xs (j && rd r t j) j
    eval ((OR  r T):xs) t j = eval xs (j || rd r t j) j
    eval ((NOT r T):xs) t j = eval xs (not (rd r t j)) j

    rd T t _ = t
    rd J _ j = j
    rd x _ _ = sensors !! fromEnum x

find1 :: [Op]
find1 = reverse . head . filter goal $ bfs nf []
  where
    nf x = (\i -> i:x) <$> genStructions
    goal x = lives (reverse x) shouldJump4 shouldNotJump4

    genStructions = do
      i <- [AND, OR, NOT]
      r <- J:[A .. D]
      pure $ i r J


lives :: Script -> [Terrain] -> [Terrain] -> Bool
lives scr tj tnj = all (evaluate scr) tj && all (not . evaluate scr) tnj

shouldJump8 :: [Terrain]
shouldJump8 = toTerrain <$> [".#######",
                             "##.##.##",
                             "..###.#.",
                             "##.#..##",
                             "#..#.###",
                             "#.##.#.#"]
shouldNotJump8 :: [Terrain]
shouldNotJump8 = toTerrain <$> ["##.#.##.",
                                "####..#.",
                                "###..#.#",
                                "##..#.##",
                                "####..##",
                                "####.##.",
                                "###.##.#",
                                "###...##",
                                "##...###",
                                "#...####",
                                "###.####",
                                "####.###" ]


find2 :: [Op]
find2 = head . filter goal $ bfs nf [] -- prog1
  where
    nf x = (\i -> x <> [i]) <$> genStructions
    goal x = lives x shouldJump8 shouldNotJump8

    genStructions = do
      i <- [AND, OR, NOT]
      r <- J:[A .. H]
      pure $ i r J

progFail :: [Op]
progFail =  [ NOT D J ]

prog1 :: [Op]
prog1 = [OR A J,
         AND B J,
         AND C J,
         NOT J J,
         AND D J]



prog2 :: [Op]
prog2 = prog1 <> [AND H J,
                  OR E J,
                  AND D J]

part1 :: IO (Either String Int)
part1 = runOne (toScript prog1 <> "WALK\n") <$> getInput

part2 :: IO (Either String Int)
part2 = runOne (toScript prog2 <> "RUN\n") <$> getInput
