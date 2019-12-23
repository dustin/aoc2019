{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day23 where

import           Data.List.Extra (chunksOf)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)

import           ComputerST      (Instructions, Paused (..), executePause,
                                  readInstructions, resumePause)

import           Search          (findRepeated)

getInput :: IO Instructions
getInput = readInstructions "input/day23"

run :: Instructions -> [(Int,Int)]
run prog = send (-1,-1) [(n,(-1,-1)) | n <- [0..49]] newNet
  where
    newNet = Map.fromList [(a, executePause [a] prog) | a <- [0..49]]
    send nv [] net                = nv : send nv [(0, nv)] net
    send _ ((255,xy):xs) net      =      send xy xs net
    send nv ((addr,(x,y)):xs) net =      send nv (xs <> readQueue p) net'
      where (Just p, net') = Map.updateLookupWithKey (const (Just . resumePause [x,y])) addr net
            readQueue = map (\[a,b,c] -> (a,(b,c))) . chunksOf 3 . pausedOuts

part1 :: IO Int
part1 = snd . head . run <$> getInput

part2 :: IO Int
part2 = fromJust . findRepeated . fmap snd . run <$> getInput
