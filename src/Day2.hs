module Day2 where

import           Control.Lens
import           Control.Monad   (guard)
import           Data.List.Extra

getInput :: IO [Int]
getInput = fmap read . words . map (\x -> if x == ',' then ' ' else x) <$> readFile "input/day2"

execute :: Int -> [Int] -> [Int]
execute x xs = case drop x xs of
                 (99:_)      -> xs
                 (1:b:c:d:_) -> execute (x + 4) $ up (+) b c d
                 (2:b:c:d:_) -> execute (x + 4) $ up (*) b c d
  where
    up o n m d = xs & ix d .~ (o (xs !! n) (xs !! m))

runWith :: Int -> Int -> [Int] -> Int
runWith a b xs = head $ execute 0 xs'
  where xs' = xs & ix 1 .~ a & ix 2 .~ b

part1 :: [Int] -> Int
part1 = runWith 12 2

part2 :: [Int] -> Int
part2 xs = head $ do
  a <- [0..100]
  b <- [0..100]

  guard $ runWith a b xs == 19690720

  pure $ 100 * a + b
