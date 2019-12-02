{-# LANGUAGE ExplicitForAll #-}

module Day2 where

import           Control.Lens
import           Control.Monad               (guard)
import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Instructions = V.Vector Int

getInput :: IO Instructions
getInput = V.fromList . fmap read . words . map (\x -> if x == ',' then ' ' else x) <$> readFile "input/day2"

-- Mutable vector in ST monad.
execute :: Instructions -> Instructions
execute ins = runST $ do
  mv <- V.thaw ins
  ex 0 mv
  V.unsafeFreeze mv

    where
      ex :: Int -> MV.MVector s Int -> ST s ()
      ex x xs = do
        i <- MV.read xs x
        case i of
          99 -> pure ()
          1  -> op4 (+)
          2  -> op4 (*)

          where
            op4 o = do
              a <- att (x + 1)
              b <- att (x + 2)
              dest <- MV.read xs (x + 3)
              MV.write xs dest (o a b)
              ex (x + 4) xs

            att i = MV.read xs =<< MV.read xs i

{- Immutable Vector version
execute' :: Int -> Instructions -> Instructions
execute' x xs = case xs V.! x of
                 99 -> xs
                 1  -> op4 (+)
                 2  -> op4 (*)
  where
    op4 o = execute' (x+4) (xs & ix dest .~ o opa opb)
      where opa   = att 1
            opb   = att 2
            dest  = xs V.! (x + 3)
            att n = xs V.! (xs V.! (x + n))
-}

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
