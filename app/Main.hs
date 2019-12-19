{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Day18

main :: IO ()
main = do
  m <- p2Input "input/day18"
  putStrLn $ displayMap m
