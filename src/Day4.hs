module Day4 where

import           Control.Applicative (liftA2)
import           Data.List           (foldl', group)

quantity :: (Int, Int) -> ([Int] -> Bool) -> Int
quantity r p = length $ pwgen r (liftA2 (&&) increasing (p . fmap length . group . show))
  where pwgen (l,h) p' = [x | x <- [l..h], p' x]
        increasing = snd . foldl' (\(prev,o) x -> (x, o && x >= prev)) ('/', True) . show

part1 :: (Int, Int) -> Int
part1 r = quantity r $ any (> 1)

part2 :: (Int, Int) -> Int
part2 r = quantity r $ (2 `elem`)
