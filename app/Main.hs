{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Day16

main :: IO ()
main = print =<< doPart2ST <$> getInput
