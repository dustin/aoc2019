{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent (threadDelay)
import qualified Data.Map.Strict    as Map
import           System.IO          (hFlush, stdout)

import           Day13

main :: IO ()
main = do
  rgs <- part2
  let (g:gs) = reverse rgs
  putStr "\ESC[?25l\ESC[2J;\ESC[0;0H"

  putStrLn $ drawGame g
  let diffs = zipWith (\(Game m1 _) (Game m2 _) -> Map.toList $ Map.differenceWith (\a b -> if a == b then Nothing else Just b) m1 m2) (g:gs) gs
  mapM_ drawUp $ diffs

  putStr "\ESC[s\ESC[2J;\ESC[0;0H"
  putStrLn $ drawGame (head rgs)
  putStr "\ESC[?25h"

  where
    drawUp :: [TilePos] -> IO ()
    drawUp l = mapM_ drawUp1 l >> hFlush stdout >> threadDelay 10000

    drawUp1 :: TilePos -> IO ()
    drawUp1 ((x,y), o) = putStr $ "\ESC[" <> show (y + 2) <> ";" <> show (x + 1) <> "H" <> f o
    f EmptySpace = " "
    f Ball       = "o"
    f Horizontal = "-"
