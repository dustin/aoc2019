{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day13 where

import           Control.Lens
import           Data.Either         (fromRight)
import           Data.List           (partition)
import           Data.List.Extra     (chunksOf)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

-- For displaying
import           Control.Concurrent  (threadDelay)
import           Control.Exception   (bracket_)
import           System.Console.ANSI
import           System.IO           (hFlush, stdout)

import           ComputerST
import           Vis

getInput :: IO Instructions
getInput = readInstructions "input/day13"

data Tile = EmptySpace | Wall | Block | Horizontal | Ball deriving (Show, Bounded, Enum, Eq)

type TilePos = ((Int, Int), Tile)

data Game = Game {
  board :: Map (Int, Int) Tile,
  score :: Int
  } deriving (Show)

decOut :: [Int] -> Game
decOut xs = Game (Map.fromList $ dec <$> chunks) (sc s)
  where
    (chunks, s) = partition (\[x,_,_] -> x >= 0) $ chunksOf 3 xs
    sc [] = 0
    sc xx = (\[_,_,x] -> x) . head $ xx
    dec :: [Int] -> TilePos
    dec [a,b,c] = ((a,b), toEnum c)

tileChar :: Tile -> Char
tileChar EmptySpace = ' '
tileChar Wall       = '|'
tileChar Block      = '█'
tileChar Horizontal = '-'
tileChar Ball       = 'o'

drawGame :: Game -> String
drawGame (Game m s) = "Score: " <> show s <> "\n" <> drawString m cf
  where
    cf pos = tileChar (Map.findWithDefault EmptySpace pos m)

part1 :: IO Int
part1 = do
  (Game m _) <- decOut . outputs . fromRight undefined . execute <$> getInput
  let bs = Map.filter isBlock m
  pure (length bs)

  where
    isBlock Block = True
    isBlock _     = False

updGame :: Game -> [Int] -> Game
updGame (Game m _) xs = Game (Map.union m' m) s
  where
    (Game m' s) = decOut xs

playGame :: Instructions -> [Game]
playGame progIn = start

  where
    start = let st@(Left (NoInput Paused{..})) = execute prog in go [(decOut pausedOuts)] st


    go gs@(g:_) (Right FinalState{..})        = updGame g outputs : gs
    go gs@(g:_) (Left (NoInput p@Paused{..})) = go (g':gs) (resume p{pausedOuts=[]} [s g'])
      where g' = updGame g pausedOuts

    prog = progIn & ix 0 .~ 2

    s g = (f Ball g) - (f Horizontal g)
    f t (Game m _) = foldr (\((x,_),t') o -> if t == t' then x else o) 0 $ Map.toList m


part2 :: IO [Game]
part2 = playGame <$> getInput

part2Animated :: IO ()
part2Animated =do
  rgs <- part2
  let (g:gs) = reverse rgs

  clearScreen
  setCursorPosition 0 0
  putStrLn $ drawGame g

  let diffs = zipWith (\(Game m1 _) (Game m2 s) -> Game (Map.differenceWith (\a b -> if a == b then Nothing else Just b) m1 m2) s) (g:gs) gs

  bracket_ hideCursor showCursor (mapM_ drawUp diffs)

  clearScreen
  setCursorPosition 0 0
  putStrLn $ drawGame (head rgs)

  where
    drawUp :: Game -> IO ()
    drawUp (Game m s) = do
      mapM_ drawUp1 (Map.toList m)
      drawScore s
      hFlush stdout
      threadDelay 10000

    drawUp1 :: TilePos -> IO ()
    drawUp1 ((x,y), o) = setCursorPosition (y+1) x >> putStr [tileChar o]

    drawScore 0 = pure ()
    drawScore s = setCursorPosition 0 7 >> print s
