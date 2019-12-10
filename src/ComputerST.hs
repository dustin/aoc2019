{-# LANGUAGE RecordWildCards #-}

module ComputerST (execute, executeWithin, executeIn, readInstructions,
                   Instructions, Termination(..),
                   executeWithinST, VMState(..), Op, Mode(..),
                   rd, wr, FinalState(..), Paused(..), resume) where

import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

import           OKComputer

type Instructions = V.Vector Int

readInstructions :: FilePath -> IO Instructions
readInstructions fn = V.fromList . fmap read . words . map (\x -> if x == ',' then ' ' else x) <$> readFile fn

type MInstructions s = V.MVector s Int

data Paused = Paused {
  pausedPC   :: Int,
  pausedIns  :: Instructions,
  pausedOuts :: [Int]
  } deriving(Eq, Show)

data Termination = NormalTermination
                 | NoInput Paused
                 | Bugger String deriving (Show, Eq)

data VMState s = VMState {
  pc      :: !Int,
  vram    :: !(MInstructions s),
  vinputs :: [Int],
  vout    :: [Int]
  }

fromPaused :: [Int] -> Paused -> ST s (VMState s)
fromPaused i Paused{..} = V.thaw pausedIns >>= \ir -> pure $ VMState pausedPC ir i pausedOuts

data FinalState = FinalState {
  ram     :: !Instructions,
  outputs :: ![Int]
  } deriving(Show, Eq)

type Op s = Modes -> VMState s -> ST s (Either Termination (VMState s))

-- op4 is a four-int operation.  The first int is the opcode.  We know that by the time we got here.
-- Next are two inputs, a and b.  The last is the destination.
--
-- We apply the given binary operation to the two values at the given
-- addresses and store the result in the desired location, returning
-- the new pc (which is old pc + 4)
op4 :: (Int -> Int -> Int) -> Op s
op4 o (m1,m2,m3) vms@VMState{..} = do
  a <- rd m1 (pc + 1) vram
  b <- rd m2 (pc + 2) vram
  wr m3 vram (pc + 3) (o a b)
  pure (Right vms{pc=pc + 4})

rd :: Mode -> Int -> MInstructions s -> ST s Int
rd Position i ram  = MV.read ram =<< MV.read ram i
rd Immediate i ram = MV.read ram i
rd _ _ _           = undefined

wr :: Mode -> MInstructions s -> Int -> Int -> ST s ()
wr Position ram dest val = MV.read ram dest >>= \dest' -> MV.write ram dest' val
wr Immediate ram dest val = MV.write ram dest val
wr _ _ _ _ = undefined

-- This function completely ignores its parameter modes.
input :: Op s
input _ vms@VMState{..}
  | null vinputs = V.unsafeFreeze vram >>= \r -> pure $ Left (NoInput (Paused pc r vout))
  | otherwise = do
      dest <- rd Immediate (pc + 1) vram
      wr Immediate vram dest (head vinputs)
      pure (Right vms{pc=pc + 2, vinputs=tail vinputs})

output :: Op s
output (m,_,_) vms@VMState{..} = do
  val <- rd m (pc + 1) vram
  pure (Right vms{pc=pc + 2, vout=vout<>[val]})

opjt :: Op s
opjt (m1,m2,_) vms@VMState{..} = do
  val <- rd m1 (pc + 1) vram
  dest <- rd m2 (pc + 2) vram
  pure (Right vms{pc=if val /= 0 then dest else  pc + 3})

opjf :: Op s
opjf (m1,m2,_) vms@VMState{..} = do
  val <- rd m1 (pc + 1) vram
  dest <- rd m2 (pc + 2) vram
  pure (Right vms{pc=if val == 0 then dest else pc + 3})

cmpfun :: (Int -> Int -> Bool) -> Op s
cmpfun f = op4 (\a b -> if f a b then 1 else 0)

runOp :: Operation -> Op s
runOp OpAdd    = op4 (+)
runOp OpMul    = op4 (*)
runOp OpIn     = input
runOp OpOut    = output
runOp OpJT     = opjt
runOp OpJF     = opjf
runOp OpLT     = cmpfun (<)
runOp OpEq     = cmpfun (==)
runOp OpSetrel = undefined -- setrel
runOp OpHalt   = const . const . pure $ Left NormalTermination

executeWithinST :: Int -> VMState s -> ST s (Either Termination FinalState)
executeWithinST 0 _ = pure . Left $ Bugger "timed out"
executeWithinST n vms@VMState{..} = do
  i <- MV.read vram pc
  let (op, modes) = decodeInstruction i
  e <- runOp op modes vms
  case e of
    Left x     -> terminate x
    Right vms' -> executeWithinST (n - 1) vms'

    where terminate NormalTermination = V.unsafeFreeze vram >>= \r -> pure $ Right (FinalState r vout)
          terminate x                 = pure $ Left x

-- Mutable vector in ST monad.
executeWithin :: Int -> Instructions -> Either Termination FinalState
executeWithin limit ins = runST $ do
  mv <- V.thaw ins
  let vms = VMState{pc=0, vram=mv, vinputs=[], vout=[]}
  either (pure . Left) (pure . Right) =<< executeWithinST limit vms

execute :: Instructions -> Either Termination FinalState
execute = executeWithin 100000

-- Mutable vector in ST monad.
executeWithinIns :: Int -> [Int] -> Instructions -> Either Termination FinalState
executeWithinIns limit invals ins = runST $ do
  mv <- V.thaw ins
  let vms = VMState{pc=0, vram=mv, vinputs=invals, vout=[]}
  either (pure . Left) (pure . Right) =<< executeWithinST limit vms

executeIn :: [Int] -> Instructions -> Either Termination FinalState
executeIn = executeWithinIns 100000

-- Resume a paused VM.
resume :: Paused -> [Int] -> Either Termination FinalState
resume p ins = runST $ do
  vms <- fromPaused ins p
  either (pure . Left) (pure . Right) =<< executeWithinST 100000 vms

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
