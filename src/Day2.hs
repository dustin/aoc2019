{-# LANGUAGE ExplicitForAll #-}

module Day2 where

import           Control.Lens
import           Control.Monad       (guard)
import qualified Data.Vector.Unboxed as V

import           Computer

getInput :: IO Instructions
getInput = V.fromList . fmap read . words . map (\x -> if x == ',' then ' ' else x) <$> readFile "input/day2"

runWith :: Int -> Int -> Instructions -> Int
runWith a b xs = V.head $ execute xs'
  where xs' = xs & ix 1 .~ a & ix 2 .~ b

part1 :: Instructions -> Int
part1 = runWith 12 2

part2 :: Instructions -> Int
part2 xs = head $ do
  a <- [0..100]
  b <- [0..100]

  guard $ runWith a b xs == 19690720

  pure $ 100 * a + b
