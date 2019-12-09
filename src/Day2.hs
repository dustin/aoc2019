module Day2 where

import           Control.Monad               (guard)
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.Either                 (fromRight)
import qualified Data.Map.Strict             as Map

import           Computer
import           Search

getInput :: IO Instructions
getInput = readInstructions "input/day2"

runWith :: Integer -> Integer -> Instructions -> Integer
runWith a b (Instructions xs) = peek 0 . ram . fromRight undefined $ execute xs'
  where xs' = Instructions (Map.union (Map.fromList [(1, a), (2, b)]) xs)

part1 :: Instructions -> Integer
part1 = runWith 12 2

part2 :: Instructions -> Integer
part2 xs = head $ do
  a <- [0..100]
  b <- [0..100]

  guard $ runWith a b xs == 19690720

  pure $ 100 * a + b

-- Parallel version of part 2
part2' :: Instructions -> Integer
part2' xs = head . filter (/= 0) . parMap rdeepseq f $ range
  where
    range = [(a,b) | a <- [1..100], b <- [1..100]]
    f (a,b) = case runWith a b xs of
                19690720 -> 100 * a + b
                _        -> 0

part2'' :: Instructions -> Integer
part2'' prog = autoBinSearch (\x -> let (n,v) = x `divMod` 100 in compare (runWith n v prog) 19690720)
