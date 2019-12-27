{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day16 where

import           Control.Monad               (replicateM_)
import           Control.Monad.ST
import           Data.Char                   (digitToInt, isDigit)
import           Data.Int
import           Data.List                   (foldl')
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

getInput :: IO [Int]
getInput = parseNum . filter isDigit <$> readFile "input/day16"

parseNum :: String -> [Int]
parseNum = fmap digitToInt

fft :: [Int] -> [Int]
fft xs = map (\p -> fftAdd (drop (p-1) xs) (rn p patFuns)) $ take (length xs) [1..]
  where fftAdd digs pat = (`mod` 10) . abs . sum . zipWith ($) (cycle pat) $ digs
        rn n = drop n . cycle . concatMap (replicate n)
        patFuns = [const 0, id, const 0, negate]

fftNaive :: [Int] -> [Int]
fftNaive xs = map (\p -> fftAddNaive xs (rn p basePat)) $ take (length xs) [1..]
  where fftAddNaive digs pat = (`mod` 10) . abs . sum . zipWith (*) digs $ cycle pat
        basePat = [0, 1, 0, -1]
        rn n = drop 1 . cycle . concatMap (replicate n)

listNum :: Num a => [a] -> a
listNum = foldl' (\o x -> x + o * 10) 0

part1 :: IO Int
part1 = listNum . take 8 . (!! 100) . iterate fft <$> getInput

doPart2 :: [Int] -> Int
doPart2 xin = let repd = concat . replicate 10000 $ xin
                  n = listNum $ take 7 xin
                  rest = drop n repd
                  hundy = iterate sums (w <$> rest) !! 100 in
                fromIntegral . listNum . take 8 $ hundy
  where w :: Int -> Int32
        w = fromIntegral

        sums :: Integral a => [a] -> [a]
        sums = fmap ((`mod` 10) . abs) . scanr1 (+)

doPart2ST :: [Int] -> Int
doPart2ST xin = runST $ do
  let vin = V.concat . replicate 10000 $ V.fromList (fromIntegral <$> xin)
      n = listNum $ take 7 xin
      rest = V.drop n vin
      lastdig = V.last vin
  mv <- V.unsafeThaw rest
  replicateM_ 100 $ fftSum mv lastdig
  ans <- traverse (MV.read mv) [0..7]
  pure $ fromIntegral . listNum $ ans

    where
      fftSum :: MV.MVector s Int32 -> Int32 -> ST s ()
      fftSum mv = compute (MV.length mv - 2)
        where
          compute (-1) _ = pure ()
          compute n c = do
                v <- MV.read mv n
                let c' = v + c
                MV.write mv n (c' `mod` 10)
                compute (n-1) c'

part2 :: IO Int
part2 = doPart2 <$> getInput
