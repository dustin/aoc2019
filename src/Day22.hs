{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
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
newDeck = Seq.fromList [0..10006]

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

part2 :: IO Int
part2 = pure 0
