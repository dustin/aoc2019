{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Day24

main :: IO ()
main = print =<< part2 200 <$> getInput "input/day24"
