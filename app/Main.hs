{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent  (threadDelay)
import           Control.Exception   (bracket_)
import qualified Data.Map.Strict     as Map
import           System.Console.ANSI
import           System.IO           (hFlush, stdout)

import           Day13

main :: IO ()
main = do
  rgs <- part2
  let (g:gs) = reverse rgs

  clearScreen
  setCursorPosition 0 0
  putStrLn $ drawGame g

  let diffs = zipWith (\(Game m1 _) (Game m2 _) -> Map.toList $ Map.differenceWith (\a b -> if a == b then Nothing else Just b) m1 m2) (g:gs) gs

  bracket_ hideCursor showCursor (mapM_ drawUp diffs)

  clearScreen
  setCursorPosition 0 0
  putStrLn $ drawGame (head rgs)

  where
    drawUp :: [TilePos] -> IO ()
    drawUp l = mapM_ drawUp1 l >> hFlush stdout >> threadDelay 10000

    drawUp1 :: TilePos -> IO ()
    drawUp1 ((x,y), o) = setCursorPosition (y+1) x >> putStr [tileChar o]
