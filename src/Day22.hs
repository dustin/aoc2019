{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day22 where

import           Control.Applicative        ((<|>))
import qualified Data.Array.Unboxed         as A
import           Data.List                  (foldl')
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Text.Megaparsec            (endBy)
import           Text.Megaparsec.Char       (space, string)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

import           AoC

data Deal = NewStack | Cut Int | Increment Int deriving Show

type Card = Int

type Cards = Seq Card

parseDeal :: Parser Deal
parseDeal = ns <|> ct <|> inc
  where
    ns = NewStack <$ "deal into new stack"
    ct = Cut <$> (string "cut " *> sd)
    inc = Increment <$> (string "deal with increment " *> decimal)

    sd = signed space decimal

getInput :: FilePath -> IO [Deal]
getInput = parseFile parseAll

parseAll :: Parser [Deal]
parseAll = parseDeal `endBy` "\n"

newDeck :: Cards
newDeck = newDeckOf 10007

newDeckOf :: Int -> Cards
newDeckOf n = Seq.fromList [0..n - 1]

apply :: [Deal] -> Cards -> Cards
apply deals cards = foldl' deal cards deals
  where
    deal :: Cards -> Deal -> Cards
    deal d NewStack = Seq.reverse d
    deal d (Cut x)
      | x > 0     = let (a, b) = Seq.splitAt x d in b <> a
      | otherwise = let (a, b) = Seq.splitAt (length d + x) d in b <> a
    deal d (Increment x) = let ns = zip [0 .. length d - 1] [x' `mod` length d | x' <- [0, x ..]] in
                             Seq.fromArray $ A.array (0, length d - 1) [(b, Seq.index d a) | (a,b) <- ns]

part1 :: IO (Maybe Int)
part1 =  Seq.findIndexL (== 2019) . flip apply newDeck <$> getInput "input/day22"

-- https://rosettacode.org/wiki/Modular_exponentiation#Haskell
powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm _ 0 _ r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

part2 :: IO Integer
part2 = do
  inp <- getInput "input/day22"
  let (a, b) = reduce inp
      x = offset * powm a deals deckSize 1
      y = b * (powm a deals deckSize 1 + deckSize - 1)
      z = powm (a - 1) (deckSize - 2) deckSize 1

  pure $ mds $ x + (y * z)

  where
    deckSize = 119315717514047
    deals = 101741582076661
    offset = 2020
    mds = (`mod` deckSize)

    reduce = foldr deal (1,0)
      where
        deal NewStack      (a,b) = (mds $ negate a, mds $ negate (b + 1))
        deal (Cut x)       (a,b) = (a, mds (b + fromIntegral x))
        deal (Increment x) (a,b) = let p = powm (fromIntegral x) (deckSize-2) deckSize 1 in
                                     (mds (a * p), mds (b * p))
