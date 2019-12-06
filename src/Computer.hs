{-# LANGUAGE RecordWildCards #-}

module Computer (execute, executeWithin, executeIn, Instructions, Termination(..), FinalState(..)) where

import           Control.Monad.ST
import qualified Data.Array                  as A
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Instructions = V.Vector Int

type MInstructions s = V.MVector s Int

data Termination = NormalTermination | Bugger String deriving (Show, Eq)

data Mode = Position | Immediate deriving (Show, Eq)

type Modes = (Mode, Mode, Mode)

data VMState s = VMState {
  pc      :: Int,
  ram     :: MInstructions s,
  inputs  :: [Int],
  outputs :: [Int]
  }

data FinalState = FinalState {
  fram     :: Instructions,
  foutputs :: [Int]
  } deriving(Show, Eq)

-- op4 is a four-int operation.  The first int is the opcode.  We know that by the time we got here.
-- Next are two addresses, a and b.  We dereference those with 'att'.
-- The last is the destination address.
--
-- We apply the given binary operation to the two values at the given
-- addresses and store the result in the desired location, returning
-- the new pc (which is old pc + 4)
op4 :: (Int -> Int -> Int) -> Modes -> VMState s -> ST s (Either Termination (VMState s))
op4 o (m1,m2,m3) vms@VMState{..} = do
  a <- rd m1 (pc + 1) ram
  b <- rd m2 (pc + 2) ram
  wr m3 ram (pc + 3) (o a b)
  pure (Right vms{pc=pc + 4})

-- pos dereferences a pointer in the machine given the address of a pointer.

rd :: Mode -> Int -> MInstructions s -> ST s Int
rd Position i ram  = MV.read ram =<< MV.read ram i
rd Immediate i ram = MV.read ram i

wr :: Mode -> MInstructions s -> Int -> Int -> ST s ()
wr Immediate ram dest val = MV.write ram dest val
wr Position ram dest val = MV.read ram dest >>= \dest' -> MV.write ram dest' val

type Op s = Modes -> VMState s -> ST s (Either Termination (VMState s))

type InstructionSet s = A.Array Int (Op s)

-- This function completely ignores its parameter modes.
store :: Modes -> VMState s -> ST s (Either Termination (VMState s))
store _ vms@VMState{..} = do
  dest <- rd Immediate (pc + 1) ram
  wr Immediate ram dest (head inputs)
  pure (Right vms{pc=pc + 2, inputs=tail inputs})

output :: Modes -> VMState s -> ST s (Either Termination (VMState s))
output (m,_,_) vms@VMState{..} = do
  val <- rd m (pc + 1) ram
  pure (Right vms{pc=pc + 2, outputs=val:outputs})

opjt :: Modes -> VMState s -> ST s (Either Termination (VMState s))
opjt (m1,m2,_) vms@VMState{..} = do
  val <- rd m1 (pc + 1) ram
  dest <- rd m2 (pc + 2) ram
  pure (Right vms{pc=if val /= 0 then dest else  pc + 3})

opjf :: Modes -> VMState s -> ST s (Either Termination (VMState s))
opjf (m1,m2,_) vms@VMState{..} = do
  val <- rd m1 (pc + 1) ram
  dest <- rd m2 (pc + 2) ram
  pure (Right vms{pc=if val == 0 then dest else pc + 3})

cmpfun :: (Int -> Int -> Bool) -> Modes -> VMState s -> ST s (Either Termination (VMState s))
cmpfun f = op4 (\a b -> if f a b then 1 else 0)

-- This is our instruction set.  It's pretty simple now, but may grow.
defaultSet :: InstructionSet s
defaultSet = A.array (0,100) [(x, op x) | x <- [0..100]]
  where
    op    99 = const . const . pure $ Left NormalTermination
    op     1 = op4 (+)
    op     2 = op4 (*)
    op     3 = store
    op     4 = output
    op     5 = opjt
    op     6 = opjf
    op     7 = cmpfun (<)
    op     8 = cmpfun (==)
    op     x = const . const . pure . Left $ Bugger ("invalid instruction: " <> show x)

amode :: Int -> Mode
amode 0 = Position
amode 1 = Immediate
amode x = error ("invalid mode: " <> show x)

modes :: Int -> (Mode, Mode, Mode)
modes x = (nmode 100, nmode 1000, nmode 10000)
  where nmode pl = amode $ x `div` pl `mod` (pl `div` 10)

executeWithinST :: Int -> VMState s -> InstructionSet s -> ST s (Either Termination FinalState)
executeWithinST 0 _ _ = pure . Left $ Bugger "timed out"
executeWithinST n vms@VMState{..} iSet = do
  i <- MV.read ram pc
  let basei = i `mod` 100
      imodes = modes i
  e <- (iSet A.! basei) imodes vms
  case e of
    Left x     -> terminate x
    Right vms' -> executeWithinST (n - 1) vms' iSet

    where terminate NormalTermination = V.unsafeFreeze ram >>= \r -> pure $ Right (FinalState r (reverse outputs))
          terminate x                 = pure $ Left x

-- Mutable vector in ST monad.
executeWithin :: Int -> Instructions -> Either Termination FinalState
executeWithin limit ins = runST $ do
  mv <- V.thaw ins
  let vms = VMState{pc=0, ram=mv, inputs=[], outputs=[]}
  either (pure . Left) (pure . Right) =<< executeWithinST limit vms defaultSet

execute :: Instructions -> Either Termination FinalState
execute = executeWithin 100000

-- Mutable vector in ST monad.
executeWithinIns :: Int -> [Int] -> Instructions -> Either Termination FinalState
executeWithinIns limit invals ins = runST $ do
  mv <- V.thaw ins
  let vms = VMState{pc=0, ram=mv, inputs=invals, outputs=[]}
  either (pure . Left) (pure . Right) =<< executeWithinST limit vms defaultSet

executeIn :: [Int] -> Instructions -> Either Termination FinalState
executeIn = executeWithinIns 100000


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
