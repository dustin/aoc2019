{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day23 where

import           Data.List.Extra (chunksOf)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)

import           ComputerST      (Instructions, Paused (..), executePause,
                                  readInstructions, resumePause)

import           AoC             (thrd)
import           Search

getInput :: IO Instructions
getInput = readInstructions "input/day23"

type Network = Map Int Paused

mkNet :: Instructions -> Network
mkNet prog = Map.fromList [(a, executePause [a] prog) | a <- [0..49]]

type Queue = [(Int, (Int, Int))]

run :: Instructions -> [(Int,Int)]
run prog = let inet = mkNet prog
           in go [(n,(-1,-1)) | n <- Map.keys inet] Nothing inet
  where
    go xs nv net = let (nv'@(Just xy), net') = sendMessages nv xs net
                   in xy : go [(0, fromJust nv')] nv' net'
    sendMessages nv [] net = (nv, net)
    sendMessages _ ((255,xy):xs) net = sendMessages (Just xy) xs net
    sendMessages nv ((addr,xy):xs) net = let (Just p, net') = send1 addr xy
                                            in sendMessages nv (xs <> readQueue p) net'
      where
        send1 to (x,y) = Map.updateLookupWithKey (const (Just . resumePause [x,y])) to net
        readQueue = map (\[a,b,c] -> (a,(b,c))) . chunksOf 3 . pausedOuts

part1 :: IO Int
part1 = snd . head . run <$> getInput

part2 :: IO Int
part2 = snd . thrd . findCycle snd . run <$> getInput
