{-# LANGUAGE GADTs #-}

module OKComputer where

data Mode = Position | Immediate | Relative deriving (Show, Eq)

type Modes = (Mode, Mode, Mode)

data Operation = OpAdd
               | OpMul
               | OpIn
               | OpOut
               | OpJT
               | OpJF
               | OpLT
               | OpEq
               | OpSetrel
               | OpHalt
               deriving (Show, Eq, Bounded, Enum)

{-# INLINE opLen #-}
opLen :: Operation -> Int
opLen OpAdd    = 4
opLen OpMul    = 4
opLen OpLT     = 4
opLen OpEq     = 4
opLen OpIn     = 2
opLen OpOut    = 2
opLen OpJT     = 3
opLen OpJF     = 3
opLen OpSetrel = 2
opLen OpHalt   = 1

{-# INLINE amode #-}
amode :: Int -> Mode
amode 0 = Position
amode 1 = Immediate
amode 2 = Relative
amode x = error ("invalid mode: " <> show x)

{-# INLINE idOP #-}
idOP :: Int -> Operation
idOP x
  | x > fromEnum (maxBound::Operation) = OpHalt
  | otherwise = toEnum (x - 1)

{-# INLINE decodeInstruction #-}
decodeInstruction :: Integral a => a -> (Operation, Modes)
decodeInstruction xin = (idOP (x `mod` 100), modes x)
  where
    x :: Int
    x = fromIntegral xin
    modes :: Int -> (Mode, Mode, Mode)
    modes n = (nmode 100, nmode 1000, nmode 10000)
      where nmode pl = amode $ n `mod` (pl*10) `div` pl
