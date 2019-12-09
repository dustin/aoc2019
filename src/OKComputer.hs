{-# LANGUAGE GADTs #-}

module OKComputer where

data Mode = Position | Immediate | Relative deriving (Show, Eq)

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

amode :: Int -> Mode
amode 0 = Position
amode 1 = Immediate
amode 2 = Relative
amode x = error ("invalid mode: " <> show x)

idOP :: Int -> Operation
idOP x
  | x >= fromEnum (maxBound::Operation) = OpHalt
  | otherwise = toEnum (x - 1)
