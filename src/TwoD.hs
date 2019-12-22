module TwoD where

type Point = (Int,Int)

around :: Point -> [Point]
around (x,y) = [(x,y-1), (x-1,y), (x+1,y), (x,y+1)]

data Dir = N | E | S | W deriving (Show, Bounded, Enum, Eq)

fwd :: Dir -> (Int,Int) -> (Int,Int)
fwd S (x,y) = (x,y+1)
fwd N (x,y) = (x,y-1)
fwd W (x,y) = (x-1,y)
fwd E (x,y) = (x+1,y)