{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day16 where

import           Control.Monad       (replicateM_)
import           Control.Monad.ST
import           Data.Char           (digitToInt, isDigit)
import           Data.List           (foldl')
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import           Data.Word

getInput :: IO [Int]
getInput = parseNum . filter isDigit <$> readFile "input/day16"

parseNum :: String -> [Int]
parseNum = fmap digitToInt

basePat :: [Int]
basePat = [0, 1, 0, -1]

repeatN :: Int -> [Int] -> [Int]
repeatN n = drop 1 . cycle . concatMap (replicate n)

fft :: [Int] -> [Int]
fft xs = map (\p -> fftAdd (drop (p-1) xs) (rn p basePat)) $ take (length xs) [1..]
  where fftAdd digs pat = (`mod` 10) . abs . sum . zipWith (*) digs $ (cycle pat)
        rn n = dropWhile (== 0) . cycle . concatMap (replicate n)

fftNaive :: [Int] -> [Int]
fftNaive xs = map (\p -> fftAddNaive xs (repeatN p basePat)) $ take (length xs) [1..]
  where fftAddNaive digs pat = (`mod` 10) . abs . sum . zipWith (*) digs $ (cycle pat)

listNum :: Num a => [a] -> a
listNum = foldl' (\o x -> x + o * 10) 0

part1 :: IO Int
part1 = listNum . take 8 . (!! 100) . iterate fft <$> getInput

doPart2 :: [Int] -> Int
doPart2 xin = let repd = concat . replicate 10000 $ xin
                  n = listNum $ take 7 xin
                  rest = drop n repd
                  hundy = iterate (scanr1 (+)) (map fromIntegral rest) !! 100 in
                fromIntegral . listNum . map (`mod` 10) . take 8 $ hundy

doPart2ST :: [Int] -> Int
doPart2ST xin = runST $ do
  let vin = V.concat . replicate 10000 $ (V.fromList $ fromIntegral <$> xin)
      n = listNum $ take 7 xin
      rest = V.drop n vin
      lastdig = V.last vin
  mv <- V.unsafeThaw rest
  replicateM_ 100 $ fftSum mv lastdig
  ans <- traverse (MV.read mv) [0..7]
  pure $ fromIntegral . listNum . map (`mod` 10) $ ans

    where
      fftSum mv ld = compute (MV.length mv - 2) ld
        where
          compute (-1) _ = pure ()
          compute n c = do
                v <- MV.read mv n
                let c' = v + c
                MV.write mv n c'
                compute (n-1) c'

part2 :: IO Int
part2 = doPart2 <$> getInput
