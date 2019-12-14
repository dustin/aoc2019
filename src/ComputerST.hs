{-# LANGUAGE RecordWildCards #-}

module ComputerST (execute, executeWithin, executeWithinIns, executeIn, readInstructions,
                   Instructions, Termination(..),
                   executeWithinST, VMState(..), Op, Mode(..),
                   rd, wr, FinalState(..), Paused(..), resume) where

import           Control.Monad.ST
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import qualified Data.Vector.Split           as VSplit
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV

import           OKComputer

type Instructions = V.Vector Int

readInstructions :: FilePath -> IO Instructions
readInstructions fn = V.fromList . fmap read . words . map (\x -> if x == ',' then ' ' else x) <$> readFile fn

type MInstructions s = V.MVector s Int

data Paused = Paused {
  pausedPC   :: Int,
  pausedRel  :: Int,
  pausedIns  :: [(Int, Instructions)],
  pausedOuts :: [Int]
  } deriving(Eq, Show)

data Termination = NormalTermination
                 | NoInput Paused
                 | Bugger String deriving (Show, Eq)

pageSize :: Int
pageSize = 8192

pageFor :: Int -> Int
pageFor = (pageSize *) . (`div` pageSize)

type Pages s = Map Int (MInstructions s)

data VMState s = VMState {
  pc      :: !Int,
  pages   :: !(Pages s),
  vinputs :: [Int],
  vout    :: [Int],
  relBase :: Int
  }

fromPaused :: [Int] -> Paused -> ST s (VMState s)
fromPaused i Paused{..} = do
  thawed <- mapM V.thaw $ fmap snd pausedIns
  pure $ VMState pausedPC (Map.fromList $ zip (fmap fst pausedIns) thawed) i pausedOuts pausedRel

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
  a <- rd m1 (pc + 1) relBase pages
  b <- rd m2 (pc + 2) relBase pages
  pages' <- wr m3 pages (pc + 3) relBase (o a b)
  pure (Right vms{pc=pc + 4, pages=pages'})

pread :: Pages s -> Int -> ST s Int
pread pages i = r (Map.lookup p pages)
  where
    p = pageFor i
    o = i - p
    r Nothing  = pure 0
    r (Just m) = MV.read m o

pwrite :: Pages s -> Int -> Int -> ST s (Pages s)
pwrite pages i v = w (Map.lookup p pages)
  where
    p = pageFor i
    o = i - p
    w Nothing = do
      m <- V.unsafeThaw $ V.generate pageSize (\x -> if x == o then v else 0)
      pure $ Map.insert p m pages
    w (Just m) = MV.write m o v >> pure pages

rd :: Mode -> Int -> Int -> Pages s -> ST s Int
rd Position i _ pages   = pread pages =<< pread pages i
rd Immediate i _ pages  = pread pages i
rd Relative i rel pages = pread pages =<< (+ rel) <$> pread pages i

wr :: Mode -> Pages s -> Int -> Int -> Int -> ST s (Pages s)
wr Position pages dest _ val = pread pages dest >>= \dest' -> pwrite pages dest' val
wr Immediate pages dest _ val = pwrite pages dest val
wr Relative pages dest rel val = pread pages dest >>= \dest' -> pwrite pages (dest' + rel) val

-- This function completely ignores its parameter modes.
input :: Op s
input (m1,_,_) vms@VMState{..}
  | null vinputs = do
      m <- mapM V.unsafeFreeze pages
      pure $ Left (NoInput (Paused pc relBase (Map.toList m) vout))
  | otherwise = do
      d <- dest <$> pread pages (pc+1)
      pages' <- pwrite pages d (head vinputs)
      pure (Right vms{pc=pc + 2, vinputs=tail vinputs, pages=pages'})
        where dest x = x + if m1 == Relative then relBase else 0

output :: Op s
output (m,_,_) vms@VMState{..} = do
  val <- rd m (pc + 1) relBase pages
  pure (Right vms{pc=pc + 2, vout=vout<>[val]})

opjt :: Op s
opjt (m1,m2,_) vms@VMState{..} = do
  val <- rd m1 (pc + 1) relBase pages
  dest <- rd m2 (pc + 2) relBase pages
  pure (Right vms{pc=if val /= 0 then dest else  pc + 3})

opjf :: Op s
opjf (m1,m2,_) vms@VMState{..} = do
  val <- rd m1 (pc + 1) relBase pages
  dest <- rd m2 (pc + 2) relBase pages
  pure (Right vms{pc=if val == 0 then dest else pc + 3})

cmpfun :: (Int -> Int -> Bool) -> Op s
cmpfun f = op4 (\a b -> if f a b then 1 else 0)

setrel :: Op s
setrel (m1,_,_) vms@VMState{..} = do
  v <- rd m1 (pc+1) relBase pages
  pure $ Right vms{pc=pc+2, relBase=relBase + v}

runOp :: Operation -> Op s
runOp OpAdd    = op4 (+)
runOp OpMul    = op4 (*)
runOp OpIn     = input
runOp OpOut    = output
runOp OpJT     = opjt
runOp OpJF     = opjf
runOp OpLT     = cmpfun (<)
runOp OpEq     = cmpfun (==)
runOp OpSetrel = setrel
runOp OpHalt   = const . const . pure $ Left NormalTermination

executeWithinST :: Int -> VMState s -> ST s (Either Termination FinalState)
executeWithinST 0 _ = pure . Left $ Bugger "timed out"
executeWithinST n vms@VMState{..} = do
  i <- pread pages pc
  let (op, modes) = decodeInstruction i
  e <- runOp op modes vms
  case e of
    Left x     -> terminate x
    Right vms' -> executeWithinST (n - 1) vms'

    where terminate NormalTermination = V.unsafeFreeze (pages Map.! 0) >>= \r -> pure $ Right (FinalState r vout)
          terminate x                 = pure $ Left x

paginate :: Instructions -> ST s (Pages s)
paginate ins = do
  let vs = filled <$> VSplit.chunksOf pageSize ins
  mv <- mapM V.unsafeThaw vs
  pure $ Map.fromList $ zip [0, pageSize ..] mv

  where filled v
          | V.length v == pageSize = v
          | otherwise = V.generate pageSize (\x -> fromMaybe 0 (v V.!? x))

-- Mutable vector in ST monad.
executeWithin :: Int -> Instructions -> Either Termination FinalState
executeWithin limit ins = runST $ do
  pages <- paginate ins
  let vms = VMState{pc=0, relBase=0, pages=pages, vinputs=[], vout=[]}
  either (pure . Left) (pure . Right) =<< executeWithinST limit vms

execute :: Instructions -> Either Termination FinalState
execute = executeWithin 100000

-- Mutable vector in ST monad.
executeWithinIns :: Int -> [Int] -> Instructions -> Either Termination FinalState
executeWithinIns limit invals ins = runST $ do
  pages <- paginate ins
  let vms = VMState{pc=0, relBase=0, pages=pages, vinputs=invals, vout=[]}
  either (pure . Left) (pure . Right) =<< executeWithinST limit vms

executeIn :: [Int] -> Instructions -> Either Termination FinalState
executeIn = executeWithinIns 100000

-- Resume a paused VM.
resume :: Paused -> [Int] -> Either Termination FinalState
resume p ins = runST $ do
  vms <- fromPaused ins p
  either (pure . Left) (pure . Right) =<< executeWithinST 1000000 vms

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
