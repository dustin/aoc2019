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

newtype Instructions = Instructions (Map Integer Integer) deriving (Show, Eq)

instance IsList Instructions where
  type Item Instructions = Integer
  fromList ins = Instructions (M.fromList $ zip [0..] ins)
  toList (Instructions ins) = M.elems ins

readInstructions :: FilePath -> IO Instructions
readInstructions fn = fromList . fmap read . words . map (\x -> if x == ',' then ' ' else x) <$> readFile fn

data Paused = Paused {
  pausedPC   :: Integer,
  pausedRel  :: Integer,
  pausedIns  :: Instructions,
  pausedOuts :: [Integer]
  } deriving(Eq, Show)

data Termination = NormalTermination
                 | NoInput Paused
                 | Bugger String deriving (Show, Eq)

data VMState = VMState {
  pc      :: !Integer,
  vram    :: !Instructions,
  vinputs :: ![Integer],
  vout    :: ![Integer],
  relBase :: !Integer
  }

fromPaused :: [Integer] -> Paused -> VMState
fromPaused i Paused{..} = VMState pausedPC pausedIns i pausedOuts pausedRel

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
  let a = rd m1 (pc + 1) relBase vram
      b = rd m2 (pc + 2) relBase vram
      ram' = wr m3 vram (pc + 3) relBase (o a b) in
    Right vms{pc=pc + 4, vram=ram'}

peek :: Integer -> Instructions -> Integer
peek k (Instructions m) =  M.findWithDefault 0 k m

poke :: Integer -> Integer -> Instructions -> Instructions
poke k v (Instructions m) = Instructions (M.insert k v m)

rd :: Mode -> Integer -> Integer -> Instructions -> Integer
rd Position i _ ram   = rd Immediate (peek i ram) 0 ram
rd Immediate i _ ram  = peek i ram
rd Relative i rel ram = peek ((peek i ram)+rel) ram

wr :: Mode -> Instructions -> Integer -> Integer -> Integer -> Instructions
wr Position ram dest _ val   = poke (rd Immediate dest 0 ram) val ram
wr Immediate ram dest _ val  = poke dest val ram
wr Relative ram dest rel val = poke ((peek dest ram)+rel) val ram

input :: Op
input (m1,_,_) vms@VMState{..}
  | null vinputs = Left (NoInput (Paused pc relBase vram vout))
  | otherwise = let pos = peek (pc + 1) vram
                    ram' = poke (dest pos) (head vinputs) vram in
                  Right vms{pc=pc + 2, vram=ram', vinputs=tail vinputs}
  where
    dest x = x + if m1 == Relative then relBase else 0


output :: Op
output (m,_,_) vms@VMState{..} =
  let val = rd m (pc + 1) relBase vram in
    Right vms{pc=pc + 2, vout=vout<>[val]}

opjt :: Op
opjt (m1,m2,_) vms@VMState{..} = let val = rd m1 (pc + 1) relBase vram
                                     dest = rd m2 (pc + 2) relBase vram in
                                   Right vms{pc=if val /= 0 then dest else  pc + 3}

opjf :: Op
opjf (m1,m2,_) vms@VMState{..} = let val = rd m1 (pc + 1) relBase vram
                                     dest = rd m2 (pc + 2) relBase vram in
                                   Right vms{pc=if val == 0 then dest else pc + 3}

cmpfun :: (Integer -> Integer -> Bool) -> Op
cmpfun f = op4 (\a b -> if f a b then 1 else 0)

setrel :: Op
setrel (m1,_,_) vms@VMState{..} = let v = rd m1 (pc+1) relBase vram in
  Right vms{pc=pc+2, relBase=relBase + v}

runOp :: Operation -> Op
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

executeWithin' :: Int -> VMState -> Either Termination FinalState
executeWithin' 0 _ = Left $ Bugger "timed out"
executeWithin' n vms@VMState{..} = let (op, modes) = decodeInstruction (peek pc vram)
                                       e = runOp op modes vms in
                                     case e of
                                       Left x     -> terminate x
                                       Right vms' -> executeWithin' (n - 1) vms'

    where terminate NormalTermination = Right $ FinalState vram vout
          terminate x                 = Left x

executeWithin :: Int -> Instructions -> Either Termination FinalState
executeWithin limit ins = let vms = VMState{pc=0, relBase=0, vram=ins, vinputs=[], vout=[]} in
                            executeWithin' limit vms

execute :: Instructions -> Either Termination FinalState
execute = executeWithin 100000

executeWithinIns :: Int -> [Integer] -> Instructions -> Either Termination FinalState
executeWithinIns limit invals ins = let vms = VMState{pc=0, relBase=0, vram=ins, vinputs=invals, vout=[]} in
                                      executeWithin' limit vms

executeIn :: [Integer] -> Instructions -> Either Termination FinalState
executeIn = executeWithinIns 100000

-- Resume a paused VM.
resume :: Paused -> [Integer] -> Either Termination FinalState
resume p ins = executeWithin' 100000 (fromPaused ins p)
