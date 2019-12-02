module Computer where

import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Instructions = V.Vector Int

-- Mutable vector in ST monad.
executeWithin :: Int -> Instructions -> Either String Instructions
executeWithin limit ins = runST $ do
  mv <- V.thaw ins
  e <- ex limit 0 mv
  case e of
    Left err -> pure (Left err)
    Right _  -> pure . Right =<< V.unsafeFreeze mv

    where
      ex :: Int -> Int -> MV.MVector s Int -> ST s (Either String ())
      ex 0 _ _ = pure $ Left "timed out"
      ex n pc xs = do
        i <- MV.read xs pc
        case i of
          99 -> pure (Right ())
          1  -> op4 (+)
          2  -> op4 (*)
          ic  -> pure $ Left ("invalid instruction at pos " <> show pc <> ": " <> show ic)

          where
            op4 o = do
              a <- att (pc + 1)
              b <- att (pc + 2)
              dest <- MV.read xs (pc + 3)
              MV.write xs dest (o a b)
              ex (n - 1) (pc + 4) xs

            att i = MV.read xs =<< MV.read xs i

execute :: Instructions -> Either String Instructions
execute = executeWithin 100000

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
