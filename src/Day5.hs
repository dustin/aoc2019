module Day5 where

import           Control.Lens
import           Control.Monad               (guard)
import           Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import           Data.Either                 (fromRight)
import qualified Data.Vector.Unboxed         as V

import           Computer

getInput :: IO Instructions
getInput = readInstructions "input/day5"
