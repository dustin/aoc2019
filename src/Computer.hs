{-# LANGUAGE RecordWildCards #-}

module Computer (execute, executeWithin, executeIn, readInstructions,
                 Instructions(..), Termination(..), defaultSet, peek, poke,
                 InstructionSet, Op, Mode(..), fromList,
                 FinalState(..), Paused(..), resume) where

import qualified Data.Array      as A
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

newtype Instructions = Instructions (Map Integer Integer) deriving (Show, Eq)

fromList :: [Integer] -> Instructions
fromList ins = Instructions (M.fromList $ zip [0..] ins)

readInstructions :: FilePath -> IO Instructions
readInstructions fn = fromList . fmap read . words . map (\x -> if x == ',' then ' ' else x) <$> readFile fn

data Paused = Paused {
  pausedPC   :: Integer,
  pausedIns  :: Instructions,
  pausedOuts :: [Integer]
  } deriving(Eq, Show)

data Termination = NormalTermination
                 | NoInput Paused
                 | Bugger String deriving (Show, Eq)

data Mode = Position | Immediate | Relative deriving (Show, Eq)

type Modes = (Mode, Mode, Mode)

data VMState = VMState {
  pc      :: !Integer,
  vram    :: !Instructions,
  vinputs :: [Integer],
  vout    :: [Integer]
  }

fromPaused :: [Integer] -> Paused -> VMState
fromPaused i Paused{..} = VMState pausedPC pausedIns i pausedOuts

data FinalState = FinalState {
  ram     :: !Instructions,
  outputs :: ![Integer]
  } deriving(Show, Eq)

type Op = Modes -> VMState -> Either Termination VMState

-- op4 is a four-int operation.  The first int is the opcode.  We know that by the time we got here.
-- Next are two inputs, a and b.  The last is the destination.
--
-- We apply the given binary operation to the two values at the given
-- addresses and store the result in the desired location, returning
-- the new pc (which is old pc + 4)
op4 :: (Integer -> Integer -> Integer) -> Op
op4 o (m1,m2,m3) vms@VMState{..} =
  let a = rd m1 (pc + 1) vram
      b = rd m2 (pc + 2) vram
      ram' = wr m3 vram (pc + 3) (o a b) in
    Right vms{pc=pc + 4, vram=ram'}

lu :: Integer -> Instructions -> Integer
lu k (Instructions m) = M.findWithDefault 0 k m

store :: Integer -> Integer -> Instructions -> Instructions
store k v (Instructions m) = Instructions (M.insert k v m)

peek :: Integer -> Instructions -> Integer
peek = lu

poke :: Integer -> Integer -> Instructions -> Instructions
poke = store

rd :: Mode -> Integer -> Instructions -> Integer
rd Position i ram  = rd Immediate (lu i ram) ram
rd Immediate i ram = lu i ram
rd Relative i ram  = undefined

wr :: Mode -> Instructions -> Integer -> Integer -> Instructions
wr Immediate ram dest val = store dest val ram
wr Position ram dest val  = store (rd Immediate dest ram) val ram
wr Relative _ _ _         = undefined

-- This function completely ignores its parameter modes.
input :: Op
input _ vms@VMState{..}
  | null vinputs = Left (NoInput (Paused pc vram vout))
  | otherwise = let dest = rd Immediate (pc + 1) vram
                    ram' = wr Immediate vram dest (head vinputs) in
                  Right vms{pc=pc + 2, vram=ram', vinputs=tail vinputs}

output :: Op
output (m,_,_) vms@VMState{..} =
  let val = rd m (pc + 1) vram in
    Right vms{pc=pc + 2, vout=vout<>[val]}

opjt :: Op
opjt (m1,m2,_) vms@VMState{..} = let val = rd m1 (pc + 1) vram
                                     dest = rd m2 (pc + 2) vram in
                                   Right vms{pc=if val /= 0 then dest else  pc + 3}

opjf :: Op
opjf (m1,m2,_) vms@VMState{..} = let val = rd m1 (pc + 1) vram
                                     dest = rd m2 (pc + 2) vram in
                                   Right vms{pc=if val == 0 then dest else pc + 3}

cmpfun :: (Integer -> Integer -> Bool) -> Op
cmpfun f = op4 (\a b -> if f a b then 1 else 0)

type InstructionSet = A.Array Integer Op

-- This is our instruction set.  It's pretty simple now, but may grow.
defaultSet :: InstructionSet
defaultSet = A.array (0,100) [(x, op x) | x <- [0..100]]
  where
    op    99 = const . const $ Left NormalTermination
    op     1 = op4 (+)
    op     2 = op4 (*)
    op     3 = input
    op     4 = output
    op     5 = opjt
    op     6 = opjf
    op     7 = cmpfun (<)
    op     8 = cmpfun (==)
    op     x = const . const . Left $ Bugger ("invalid instruction: " <> show x)

amode :: Integer -> Mode
amode 0 = Position
amode 1 = Immediate
amode x = error ("invalid mode: " <> show x)

modes :: Integer -> (Mode, Mode, Mode)
modes x = (nmode 100, nmode 1000, nmode 10000)
  where nmode pl = amode $ x `div` pl `mod` (pl `div` 10)

executeWithin' :: Int -> VMState -> InstructionSet -> Either Termination FinalState
executeWithin' 0 _ _ = Left $ Bugger "timed out"
executeWithin' n vms@VMState{..} iSet = let i = lu pc vram
                                            basei = i `mod` 100
                                            imodes = modes i
                                            e = (iSet A.! basei) imodes vms in
                                          case e of
                                            Left x     -> terminate x
                                            Right vms' -> executeWithin' (n - 1) vms' iSet

    where terminate NormalTermination = Right $ FinalState vram vout
          terminate x                 = Left x

-- Mutable vector in ST monad.
executeWithin :: Int -> Instructions -> Either Termination FinalState
executeWithin limit ins = let vms = VMState{pc=0, vram=ins, vinputs=[], vout=[]} in
                            executeWithin' limit vms defaultSet

execute :: Instructions -> Either Termination FinalState
execute = executeWithin 100000

-- Mutable vector in ST monad.
executeWithinIns :: Int -> [Integer] -> Instructions -> Either Termination FinalState
executeWithinIns limit invals ins = let vms = VMState{pc=0, vram=ins, vinputs=invals, vout=[]} in
                                      executeWithin' limit vms defaultSet

executeIn :: [Integer] -> Instructions -> Either Termination FinalState
executeIn = executeWithinIns 100000

-- Resume a paused VM.
resume :: Paused -> [Integer] -> Either Termination FinalState
resume p ins = executeWithin' 100000 (fromPaused ins p) defaultSet

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
