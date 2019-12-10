{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Computer (execute, executeWithin, executeIn, readInstructions,
                 Instructions(..), Termination(..), peek, poke,
                 executeWithinIns,
                 Op, Mode(..), fromList,
                 FinalState(..), Paused(..), resume) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           GHC.Exts        (IsList (..))

import           OKComputer

newtype Instructions a = Instructions (Map a a) deriving (Show, Eq)

instance Integral a => IsList (Instructions a) where
  type Item (Instructions a) = a
  fromList ins = Instructions (M.fromList $ zip [0..] ins)
  toList (Instructions ins) = M.elems ins

readInstructions :: (Integral a, Read a) => FilePath -> IO (Instructions a)
readInstructions fn = fromList . fmap read . words . map (\x -> if x == ',' then ' ' else x) <$> readFile fn

data Paused a = Paused {
  pausedPC   :: a,
  pausedRel  :: a,
  pausedIns  :: Instructions a,
  pausedOuts :: [a]
  } deriving(Eq, Show)

data Termination a = NormalTermination
                 | NoInput (Paused a)
                 | Bugger String deriving (Show, Eq)

data VMState a = VMState {
  pc      :: !a,
  vram    :: !(Instructions a),
  vinputs :: ![a],
  vout    :: ![a],
  relBase :: !a
  }

fromPaused :: [a] -> Paused a -> VMState a
fromPaused i Paused{..} = VMState pausedPC pausedIns i pausedOuts pausedRel

data FinalState a = FinalState {
  ram     :: !(Instructions a),
  outputs :: ![a]
  } deriving(Show, Eq)

type Op a = Modes -> VMState a -> Either (Termination a) (VMState a)

-- op4 is a four-int operation.  The first int is the opcode.  We know that by the time we got here.
-- Next are two inputs, a and b.  The last is the destination.
--
-- We apply the given binary operation to the two values at the given
-- addresses and store the result in the desired location, returning
-- the new pc (which is old pc + 4)
op4 :: Integral a => (a -> a -> a) -> Op a
op4 o (m1,m2,m3) vms@VMState{..} =
  let a = rd m1 (pc + 1) relBase vram
      b = rd m2 (pc + 2) relBase vram
      ram' = wr m3 vram (pc + 3) relBase (o a b) in
    Right vms{pc=pc + 4, vram=ram'}

peek :: Integral a => a -> Instructions a -> a
peek k (Instructions m) =  M.findWithDefault 0 k m

poke :: Ord a => a -> a -> Instructions a -> Instructions a
poke k v (Instructions m) = Instructions (M.insert k v m)

rd :: Integral a => Mode -> a -> a -> Instructions a -> a
rd Position i _ ram   = rd Immediate (peek i ram) 0 ram
rd Immediate i _ ram  = peek i ram
rd Relative i rel ram = peek ((peek i ram)+rel) ram

wr :: Integral a => Mode -> Instructions a -> a -> a -> a -> Instructions a
wr Position ram dest _ val   = poke (rd Immediate dest 0 ram) val ram
wr Immediate ram dest _ val  = poke dest val ram
wr Relative ram dest rel val = poke ((peek dest ram)+rel) val ram

input :: Integral a => Op a
input (m1,_,_) vms@VMState{..}
  | null vinputs = Left (NoInput (Paused pc relBase vram vout))
  | otherwise = let pos = peek (pc + 1) vram
                    ram' = poke (dest pos) (head vinputs) vram in
                  Right vms{pc=pc + 2, vram=ram', vinputs=tail vinputs}
  where
    dest x = x + if m1 == Relative then relBase else 0


output :: Integral a => Op a
output (m,_,_) vms@VMState{..} =
  let val = rd m (pc + 1) relBase vram in
    Right vms{pc=pc + 2, vout=vout<>[val]}

opjt :: Integral a => Op a
opjt (m1,m2,_) vms@VMState{..} = let val = rd m1 (pc + 1) relBase vram
                                     dest = rd m2 (pc + 2) relBase vram in
                                   Right vms{pc=if val /= 0 then dest else  pc + 3}

opjf :: Integral a => Op a
opjf (m1,m2,_) vms@VMState{..} = let val = rd m1 (pc + 1) relBase vram
                                     dest = rd m2 (pc + 2) relBase vram in
                                   Right vms{pc=if val == 0 then dest else pc + 3}

cmpfun :: Integral a => (a -> a -> Bool) -> Op a
cmpfun f = op4 (\a b -> if f a b then 1 else 0)

setrel :: Integral a => Op a
setrel (m1,_,_) vms@VMState{..} = let v = rd m1 (pc+1) relBase vram in
  Right vms{pc=pc+2, relBase=relBase + v}

runOp :: Integral a => Operation -> Op a
runOp OpAdd    = op4 (+)
runOp OpMul    = op4 (*)
runOp OpIn     = input
runOp OpOut    = output
runOp OpJT     = opjt
runOp OpJF     = opjf
runOp OpLT     = cmpfun (<)
runOp OpEq     = cmpfun (==)
runOp OpSetrel = setrel
runOp OpHalt   = const . const $ Left NormalTermination

executeWithin' :: Integral a => Int -> VMState a -> Either (Termination a) (FinalState a)
executeWithin' 0 _ = Left $ Bugger "timed out"
executeWithin' n vms@VMState{..} = let (op, modes) = decodeInstruction (peek pc vram)
                                       e = runOp op modes vms in
                                     case e of
                                       Left x     -> terminate x
                                       Right vms' -> executeWithin' (n - 1) vms'

    where terminate NormalTermination = Right $ FinalState vram vout
          terminate x                 = Left x

executeWithin :: Integral a => Int -> Instructions a -> Either (Termination a) (FinalState a)
executeWithin limit ins = let vms = VMState{pc=0, relBase=0, vram=ins, vinputs=[], vout=[]} in
                            executeWithin' limit vms

execute :: Integral a => Instructions a -> Either (Termination a) (FinalState a)
execute = executeWithin 100000

executeWithinIns :: Integral a => Int -> [a] -> Instructions a -> Either (Termination a) (FinalState a)
executeWithinIns limit invals ins = let vms = VMState{pc=0, relBase=0, vram=ins, vinputs=invals, vout=[]} in
                                      executeWithin' limit vms

executeIn :: Integral a => [a] -> Instructions a -> Either (Termination a) (FinalState a)
executeIn = executeWithinIns 100000

-- Resume a paused VM.
resume :: Integral a => Paused a -> [a] -> Either (Termination a) (FinalState a)
resume p ins = executeWithin' 100000 (fromPaused ins p)
