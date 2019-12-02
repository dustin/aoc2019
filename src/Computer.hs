module Computer where

import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Instructions = V.Vector Int

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
