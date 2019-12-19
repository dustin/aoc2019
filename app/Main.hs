{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Day19
import           System.Environment

main :: IO ()
main = do
  [l, h] <- fmap read <$> getArgs
  -- tryOne l h

  prog <- getInput
  let !g = extractGrid prog ((0, 0), (l, h))
  putStrLn $ "Processing is done with " <> show (length g) <> " element thing"
  pngGrid "day18b.png" $! g
