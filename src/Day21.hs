{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day21 where

import           Data.Char  (chr, ord)

import           ComputerST

getInput :: IO Instructions
getInput = readInstructions "input/day21"

runOne :: String -> Instructions -> Either String Int
runOne ins prog = case executeWithinIns 1000000 (fmap ord ins) prog of
                    Right (FinalState{outputs}) -> parse outputs
                    Left x -> error ("Unexpected termination: " <> show x)
  where parse oot
          | all (< 128) oot = Left (fmap chr oot)
          | otherwise = Right (last oot)

doRun :: String -> IO ()
doRun s = do
  prog <- getInput
  case runOne s prog of
    Right x -> print x
    Left e  -> putStr e

progFail :: String
progFail = unlines [ "NOT D J"
                   , "WALK"]

{-
!A || ((!B || !C) && D)


(!C && D) || A
NOT C J
AND D J
NOT A T
OR T J
WALK

(!C && D && (E || H)) || A || (!B && !E)
NOT C J
AND D J
NOT H T
NOT T T
OR E T
AND T J
NOT A T
OR T J
NOT B T
NOT T T
OR E T
NOT T T
OR T J
RUN

-}

prog1 :: String
prog1 = unlines [ "OR  A J"
                , "AND B J"
                , "AND C J"
                , "NOT J J"
                , "AND D J"
                ]

prog2 :: String
prog2 = prog1 <> unlines [ "NOT E T"
                         , "NOT T T"
                         , "OR H T"
                         , "AND T J"
                         ]

part1 :: IO (Either String Int)
part1 = runOne (prog1 <> "WALK\n") <$> getInput

part2 :: IO (Either String Int)
part2 = runOne (prog2 <> "RUN\n") <$> getInput
