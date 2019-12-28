{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day13 where

import           Control.Applicative ((<*>))
import           Control.Lens
import           Data.Either         (fromRight)
import           Data.List           (partition)
import           Data.List.Extra     (chunksOf, maximumOn)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

-- For displaying
import           Control.Concurrent  (threadDelay)
import           System.Console.ANSI
import           System.IO           (hFlush, stdout)

import           Advent.TwoD
import           Advent.Vis
import           ComputerST          (FinalState (..), Instructions,
                                      Paused (..), Termination (..), execute,
                                      readInstructions, resume)

getInput :: IO Instructions
getInput = readInstructions "input/day13"

data Tile = EmptySpace | Wall | Block | Horizontal | Ball deriving (Show, Bounded, Enum, Eq)

type TilePos = (Point, Tile)

data Game = Game {
  board :: Map Point Tile,
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
    start = let st@(Left (NoInput Paused{..})) = execute prog in go [decOut pausedOuts] st


    go gs@(g:_) (Right FinalState{..})        = updGame g outputs : gs
    go gs@(g:_) (Left (NoInput p@Paused{..})) = go (g':gs) (resume [s g'] p)
      where g' = updGame g pausedOuts

    prog = progIn & ix 0 .~ 2

    s g = f Ball g - f Horizontal g
    f t (Game m _) = foldr (\((x,_),t') o -> if t == t' then x else o) 0 $ Map.toList m


part2 :: IO [Game]
part2 = playGame <$> getInput

animateGame :: [Game] -> IO ()
animateGame (g:gs) = do
  clearScreen
  setCursorPosition 0 0
  putStrLn $ drawGame g

  let diffs = zipWith (\(Game m1 _) (Game m2 s) -> Game (Map.differenceWith (\a b -> if a == b then Nothing else Just b) m1 m2) s) (g:gs) gs

  withHiddenCursor $ mapM_ drawUp diffs

  clearScreen
  setCursorPosition 0 0
  putStrLn $ drawGame (last gs)

  where
    drawUp :: Game -> IO ()
    drawUp (Game m s) = do
      mapM_ drawUp1 (Map.toList m)
      drawScore s
      hFlush stdout
      threadDelay 100000

    drawUp1 :: TilePos -> IO ()
    drawUp1 ((x,y), o) = setCursorPosition (y+1) x >> putStr [tileChar o]

    drawScore 0 = pure ()
    drawScore s = setCursorPosition 0 7 >> print s

part2Animated :: IO ()
part2Animated = animateGame =<< reverse <$> part2

type Pages = [(Int, Instructions)]

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 a b c d e = a <$> b <*> c <*> d <*> e

bxPos, byPos, bvxPos, bvyPos :: Int
bxPos = 388
byPos = 389
bvxPos = 390
bvyPos = 391

hackGame :: Instructions -> [Game]
hackGame progIn = start

  where
    start = let st@(Left (NoInput Paused{..})) = execute prog in go [decOut pausedOuts] st


    go gs@(g:_) (Right FinalState{..})        = updGame g outputs : gs
    go gs@(g:_) (Left (NoInput p@Paused{..})) = let (ng, np) = hk pausedIns in
      go (ng:gs) (resume [0] p{pausedIns=np})

      where g' = updGame g pausedOuts
            hk :: Pages -> (Game, Pages)
            hk pages = let bst@(x,y,_,_) = bstat pages
                           (lx,ly) = lowBlock g'
                           dir = if lx <= 2 then 1 else (-1)
                           np@(nx,ny,_,_) = if y < ly+1 then bst
                                            else (lx + dir, ly+1, -dir, -1)
                       in
                         if bst == np then (g', pages)
                         else
                           let (Game m s) = g'
                               m' = Map.insert (x,y) EmptySpace m
                               m'' = Map.insert (nx,ny) Ball m' in
                             (Game m'' s, wra pages np)

            lowBlock :: Game -> Point
            lowBlock (Game m _) = fst . maximumOn (snd.fst) . filter match $ Map.toList m
              where match (_,Block) = True
                    match _         = False

            bstat :: Pages -> (Int,Int,Int,Int)
            bstat = liftA4 (,,,) bx by bvx bvy
            bx = rd 388
            by = rd 389
            bvx = rd 390
            bvy = rd 391

            rd :: Int -> Pages -> Int
            rd n pages = head $ pages ^.. ix 0 . _2 . ix n

            wra :: Pages -> (Int,Int,Int,Int) -> Pages
            wra pages (nx,ny,nvx,nvy) = let p2 = wr pages bxPos nx
                                            p3 = wr p2 byPos ny
                                            p4 = wr p3 bvxPos nvx in
                                          wr p4 bvyPos nvy

            wr :: Pages -> Int -> Int -> Pages
            wr pages n v = pages & ix 0 . _2 . ix n .~ v
    go a b = error (show a <> " - " <> show b)

    prog = progIn & ix 0 .~ 2

hack :: IO ()
hack = do
  prog <- getInput
  let rgs = hackGame prog
  -- putStrLn $ drawGame (head rgs)
  animateGame $ reverse rgs
