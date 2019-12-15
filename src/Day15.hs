{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day15 where

import           Control.Concurrent  (threadDelay)
import           Control.Monad.State
import           Data.Graph.AStar
import qualified Data.HashSet        as HS
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromMaybe)
import           System.Console.ANSI
import           System.IO           (hFlush, stdout)

import           ComputerST
import           Vis

data Dir = N | E | S | W deriving (Show, Bounded, Enum, Eq)

unDir :: Dir -> Int
unDir N = 1
unDir S = 2
unDir W = 3
unDir E = 4

otherDirs :: Dir -> [Dir]
otherDirs d = filter (/= d) [minBound..]

type World = Map (Int,Int) Char

displayMap :: World -> String
displayMap m = drawString m (\p -> Map.findWithDefault ' ' p m)

{-
0: The repair droid hit a wall. Its position has not changed.
1: The repair droid has moved one step in the requested direction.
2: The repair droid has moved one step in the requested direction;
   its new position is the location of the oxygen system.
-}

data BotState = BotState {
  world   :: World,
  stateAt :: Map (Int,Int) Paused
  }

type Excursion = State BotState

fwd :: Dir -> (Int,Int) -> (Int,Int)
fwd S (x,y) = (x,y+1)
fwd N (x,y) = (x,y-1)
fwd W (x,y) = (x-1,y)
fwd E (x,y) = (x+1,y)

around :: (Int,Int) -> [(Dir,(Int,Int))]
around p = [(d,fwd d p) | d <- [minBound..]]

neighbors :: (Int,Int) -> Excursion [(Int,Int)]
neighbors point = do
  BotState{..} <- get
  let p = stateAt Map.! point
      nps = map (\(d,np) -> (np, runOne p [unDir d])) $ around point
      chs = map (\(np, Paused{..}) -> (np, ch pausedOuts)) nps
      oks = filter (\(_,x) -> x `elem` [' ', 'O']) chs
  upd nps chs
  pure $ map fst oks

  where
    upd nps chs = modify mf
      where mf b@BotState{..} = b{stateAt=Map.union stateAt (Map.fromList nps),
                                  world=Map.union world (Map.fromList chs)}
    ch :: [Int] -> Char
    ch [0] = '█'
    ch [1] = ' '
    ch [2] = 'O'
    ch x   = error ("Unexpected input: " <> show x)

runOne :: Paused -> [Int] -> Paused
runOne p@Paused{..} ins =
  case resume p{pausedOuts=[]} ins of
    Left (NoInput p') -> p'
    x                 -> error ("Unexpected termination: " <> show x)

runSearch :: Instructions -> ((Int,Int) -> Excursion Bool) -> (Maybe [(Int,Int)], BotState)
runSearch prog goalf = runState (go (execute prog)) (BotState (Map.singleton (0,0) ' ') mempty)
  where
    go :: Either Termination FinalState -> Excursion (Maybe [(Int,Int)])
    go (Right _)            = pure Nothing

    go (Left (NoInput p)) = do
      modify (\b@BotState{..} -> b{stateAt=Map.singleton (0,0) p})
      aStarM nf (const . const $ pure (1::Int)) (const $ pure 0) goalf (pure (0,0))

    nf point = HS.fromList <$> neighbors point

findPath :: Instructions -> Maybe [(Int,Int)]
findPath prog = fst $ runSearch prog goalf
  where
    goalf point = do
      m <- gets world
      pure (Map.lookup point m == Just 'O')

flood :: Instructions -> (Int, [[(Int,Int)]])
flood prog = go 0 startPs wm [startPs]
  where
    startPs = ns [start] wm
    go n points m vs
      | (null . filter (== ' ') . Map.elems) m = (n, vs)
      | otherwise = go (n+1) np up (np:vs)
      where
        np = ns points up
        up = Map.union (Map.fromList [(p,'O') | p <- points]) m

    dd = HS.toList . HS.fromList

    ns points m = dd $ filter (\p -> Map.findWithDefault '#' p m == ' ') $ concatMap (fmap snd . around) points

    (_, botState) = runSearch prog (const $ pure False)
    wm = world botState
    (start,_) = head . filter (\(_,c) -> c == 'O') $ Map.toList wm

getInput :: IO Instructions
getInput = readInstructions "input/day15"

part1 :: IO (Maybe Int)
part1 = fmap length . findPath <$> getInput

animate1 :: IO ()
animate1 = do
  prog <- getInput
  let wholeWorld = world . snd . runSearch prog $ (const $ pure False)
      drawSpec@DrawSpec{..} = mkDrawSpec wholeWorld
      path = fromMaybe [] (findPath prog)

  clearScreen
  setCursorPosition 0 0
  putStrLn $ displayMap wholeWorld
  withHiddenCursor $ mapM_ (breadcrumbs drawSpec) path
  setCursorPosition height 0

  where
    breadcrumbs DrawSpec{..} (x,y) = do
      setCursorPosition (invTransY y) (invTransX x)
      putStr "."
      hFlush stdout
      threadDelay 100000

part2 :: IO Int
part2 = fst . flood <$> getInput

animate2 :: IO ()
animate2 = do
  prog <- getInput
  let wholeWorld = world . snd . runSearch prog $ (const $ pure False)
      drawSpec@DrawSpec{..} = mkDrawSpec wholeWorld
      (_, paths) = flood prog

  clearScreen
  setCursorPosition 0 0
  putStrLn $ displayMap wholeWorld
  withHiddenCursor $ mapM_ (breadcrumbs drawSpec) (reverse paths)
  setCursorPosition height 0
  setSGR [Reset]

  where
    breadcrumbs DrawSpec{..} ps = do
      mapM_ showOne ps
      hFlush stdout
      threadDelay 100000

      where showOne (x,y) = do
              setCursorPosition (invTransY y) (invTransX x)
              setSGR [SetColor Foreground Vivid Blue]
              putStr "O"
