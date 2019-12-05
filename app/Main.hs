{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Day4

genCount "theCount" (264360, 746325) (2 `elem`)

main :: IO ()
main = print theCount
