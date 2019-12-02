module Day2 where

import           Control.Lens
import           Control.Monad       (guard)
import qualified Data.Vector.Unboxed as V

type Instructions = V.Vector Int

getInput :: IO Instructions
getInput = V.fromList . fmap read . words . map (\x -> if x == ',' then ' ' else x) <$> readFile "input/day2"

execute :: Int -> Instructions -> Instructions
execute x xs = case xs V.! x of
                 99 -> xs
                 1  -> op4 (+)
                 2  -> op4 (*)
  where
    op4 o = execute (x+4) (xs & ix dest .~ o opa opb)
      where opa   = att 1
            opb   = att 2
            dest  = xs V.! (x + 3)
            att n = xs V.! (xs V.! (x + n))

runWith :: Int -> Int -> Instructions -> Int
runWith a b xs = V.head $ execute 0 xs'
  where xs' = xs & ix 1 .~ a & ix 2 .~ b

part1 :: Instructions -> Int
part1 = runWith 12 2

part2 :: Instructions -> Int
part2 xs = head $ do
  a <- [0..100]
  b <- [0..100]

  guard $ runWith a b xs == 19690720

  pure $ 100 * a + b
