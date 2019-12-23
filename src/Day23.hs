{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day23 where

import           Data.List.Extra (chunksOf)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           ComputerST      (Instructions, Paused (..), Termination (..),
                                  executeIn, readInstructions, resume)

import           Search

getInput :: IO Instructions
getInput = readInstructions "input/day23"

runOne :: [Int] -> Paused -> Paused
runOne ins p@Paused{..} =
  case resume p ins of
    Left (NoInput p') -> p'
    x                 -> error ("Unexpected termination: " <> show x)

type Network = Map Int Paused

mkNet :: Instructions -> Network
mkNet prog = Map.fromList $ map (\a -> (a, start a)) [0..49]
  where start a = let (Left (NoInput p)) = executeIn [a, -1] prog in p

type Queue = [(Int, (Int,Int))]

readQueue :: Paused -> Queue
readQueue = map (\[a,b,c] -> (a,(b,c))) . chunksOf 3 . pausedOuts

collectOut :: Network -> (Queue, Maybe (Int,Int))
collectOut = Map.foldr doq (mempty, Nothing)
  where
    doq :: Paused -> (Queue, Maybe (Int,Int)) -> (Queue, Maybe (Int,Int))
    doq p (q, nv) = (queue <> q, nat queue nv)
      where
        queue = readQueue p
        nat [] x               = x
        nat ((255,(x,y)):xs) _ = nat xs (Just (x,y))
        nat (_:xs) n           = nat xs n

runWithNAT :: Instructions -> [(Int,Int)]
runWithNAT prog = let inet = mkNet prog
                      (startQ, startNV) = collectOut inet
                  in go startQ startNV inet
  where
    go :: Queue -> Maybe (Int,Int) -> Network -> [(Int,Int)]
    go xs nv net = let (nv'@(Just xy), net') = sendMessages nv xs net
                   in xy : feed0 nv' net'

    feed0 nv@(Just (x,y)) net = let (Just p, net') =
                                      Map.updateLookupWithKey (\_ p' -> Just $ runOne [x,y] p') 0 net
                                    nq = readQueue p
                                in go nq nv net'

    sendMessages :: Maybe (Int,Int) -> Queue -> Network -> (Maybe (Int,Int), Network)
    sendMessages nv [] net = (nv, net)
    sendMessages _ ((255,xy):xs) net = sendMessages (Just xy) xs net
    sendMessages nv ((addr,xy):xs) net = let (Just p, net') = send1 addr xy
                                         in sendMessages nv (xs <> readQueue p) net'
      where
        send1 :: Int -> (Int,Int) -> (Maybe Paused, Network)
        send1 to (x,y) = Map.updateLookupWithKey (\_ p -> Just $ runOne [x,y] p) to net

part1 :: IO Int
part1 = snd . head . runWithNAT <$> getInput

part2 :: IO Int
part2 = do
  prog <- getInput
  let (_,_,n) = findCycle snd $ runWithNAT prog
  pure $ snd n
