{-# LANGUAGE LambdaCase #-}

module Computer where

import           Control.Monad.ST
import qualified Data.Array                  as A
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Instructions = V.Vector Int

type MInstructions s = V.MVector s Int

-- op4 is a four-int operation.  The first int is the opcode.  We know that by the time we got here.
-- Next are two addresses, a and b.  We dereference those with 'att'.
-- The last is the destination address.
--
-- We apply the given binary operation to the two values at the given
-- addresses and store the result in the desired location, returning
-- the new pc (which is old pc + 4)
op4 :: (Int -> Int -> Int) -> Int -> MInstructions s -> ST s (Maybe Int)
op4 o pc ram = do
  a <- att (pc + 1) ram
  b <- att (pc + 2) ram
  dest <- MV.read ram (pc + 3)
  MV.write ram dest (o a b)
  pure (Just (pc + 4))

-- att dereferences a pointer in the machine given the address of a pointer.
att :: Int -> MInstructions s -> ST s Int
att i ram = MV.read ram =<< MV.read ram i

type Op s = Int -> MInstructions s -> ST s (Maybe Int)

type InstructionSet s = A.Array Int (Maybe (Op s))

-- This is our instruction set.  It's pretty simple now, but may grow.
defaultSet :: InstructionSet s
defaultSet = A.array (0,100) [(x, op x) | x <- [0..100]]
  where
    op 99 = Just $ \_ _ -> pure Nothing
    op 1  = Just $ op4 (+)
    op 2  = Just $ op4 (*)
    op _  = Nothing

executeWithinST :: Int -> Int -> InstructionSet s -> MInstructions s -> ST s (Either String ())
executeWithinST 0 _ _ _ = pure $ Left "timed out"
executeWithinST n pc iSet ram = do
  i <- MV.read ram pc
  case iSet A.! i of
    Nothing -> pure $ Left ("invalid instruction at pos " <> show pc <> ": " <> show i)
    Just op -> op pc ram >>= maybe (pure (Right ())) (\o -> executeWithinST (n - 1) o iSet ram)

-- Mutable vector in ST monad.
executeWithin :: Int -> Instructions -> Either String Instructions
executeWithin limit ins = runST $ do
  mv <- V.thaw ins
  either (pure . Left) (const $ pure . Right =<< V.unsafeFreeze mv) =<< executeWithinST limit 0 defaultSet mv

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
