module Day1 where

getInput :: IO [Int]
getInput = fmap read . lines <$> readFile "input/day1"

fuelReq :: Int -> Int
fuelReq x = x `div` 3  - 2

fuelFuelReq :: Int -> Int
fuelFuelReq = sum . tail . takeWhile (> 0) . iterate fuelReq

fuelReqs :: (Int -> Int) -> [Int] -> Int
fuelReqs f = sum . fmap f
-- fuelReqs f = foldr ((+) . f) 0
-- fuelReqs f = getSum . foldMap (pure . f)

part1 :: [Int] -> Int
part1 = fuelReqs fuelReq

part2 :: [Int] -> Int
part2 = fuelReqs fuelFuelReq
