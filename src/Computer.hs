{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Computer (execute, executeWithin, Instructions, Termination(..)) where

import           Control.Monad.ST
import qualified Data.Array                  as A
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Instructions = V.Vector Int

type MInstructions s = V.MVector s Int

data Termination = NormalTermination | Bugger String deriving (Show, Eq)

data VMState s = VMState {
  pc  :: Int,
  ram :: MInstructions s
  }

-- op4 is a four-int operation.  The first int is the opcode.  We know that by the time we got here.
-- Next are two addresses, a and b.  We dereference those with 'att'.
-- The last is the destination address.
--
-- We apply the given binary operation to the two values at the given
-- addresses and store the result in the desired location, returning
-- the new pc (which is old pc + 4)
op4 :: (Int -> Int -> Int) -> VMState s -> ST s (Either Termination (VMState s))
op4 o vms@VMState{..} = do
  a <- att (pc + 1) ram
  b <- att (pc + 2) ram
  dest <- MV.read ram (pc + 3)
  MV.write ram dest (o a b)
  pure (Right vms{pc=pc + 4})

-- att dereferences a pointer in the machine given the address of a pointer.
att :: Int -> MInstructions s -> ST s Int
att i ram = MV.read ram =<< MV.read ram i

type Op s = VMState s -> ST s (Either Termination (VMState s))

type InstructionSet s = A.Array Int (Op s)

-- This is our instruction set.  It's pretty simple now, but may grow.
defaultSet :: InstructionSet s
defaultSet = A.array (0,100) [(x, op x) | x <- [0..100]]
  where
    op 99 = const . pure $ Left NormalTermination
    op 1  = op4 (+)
    op 2  = op4 (*)
    op x  = const . pure . Left $ Bugger ("invalid instruction: " <> show x)

executeWithinST :: Int -> VMState s -> InstructionSet s -> ST s (Either Termination ())
executeWithinST 0 _ _ = pure . Left $ Bugger "timed out"
executeWithinST n vms@VMState{..} iSet = do
  i <- MV.read ram pc
  (iSet A.! i) vms >>= either (pure . terminate) (\vms' -> executeWithinST (n - 1) vms' iSet)
    where terminate NormalTermination = Right ()
          terminate x                 = Left x

-- Mutable vector in ST monad.
executeWithin :: Int -> Instructions -> Either Termination Instructions
executeWithin limit ins = runST $ do
  mv <- V.thaw ins
  let vms = VMState{pc=0, ram=mv}
  either (pure . Left) (const $ pure . Right =<< V.unsafeFreeze mv) =<< executeWithinST limit vms defaultSet

execute :: Instructions -> Either Termination Instructions
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
