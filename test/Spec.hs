{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty

import qualified AoCTests
import qualified ComputerSTTests
import qualified ComputerTests
import qualified Day10Tests
import qualified Day11Tests
import qualified Day12Tests
import qualified Day1Tests
import qualified Day2Tests
import qualified Day3Tests
import qualified Day4Tests
import qualified Day5Tests
import qualified Day6Tests
import qualified Day7Tests
import qualified Day8Tests
import qualified Day9Tests
import qualified OKComputerTests
import qualified SearchTests
import qualified VisTests

tests :: [TestTree]
tests = [
  testGroup "aoc" AoCTests.tests,
  testGroup "search" SearchTests.tests,
  testGroup "vis" VisTests.tests,
  testGroup "OK computer" OKComputerTests.tests,
  testGroup "computer" ComputerTests.tests,
  testGroup "computer ST" ComputerSTTests.tests,
  testGroup "day1" Day1Tests.tests,
  testGroup "day2" Day2Tests.tests,
  testGroup "day3" Day3Tests.tests,
  testGroup "day4" Day4Tests.tests,
  testGroup "day5" Day5Tests.tests,
  testGroup "day6" Day6Tests.tests,
  testGroup "day7" Day7Tests.tests,
  testGroup "day8" Day8Tests.tests,
  testGroup "day9" Day9Tests.tests,
  testGroup "day10" Day10Tests.tests,
  testGroup "day11" Day11Tests.tests,
  testGroup "day12" Day12Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
