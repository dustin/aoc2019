{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Day21

main :: IO ()
main = do
  let scr = toScript find1
  putStrLn scr
  doRun (scr<>"WALK\n")
