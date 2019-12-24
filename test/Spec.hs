{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty

import qualified AoCTests
import qualified ComputerSTTests
import qualified ComputerTests
import qualified Day10Tests
import qualified Day11Tests
import qualified Day12Tests
import qualified Day13Tests
import qualified Day14Tests
import qualified Day15Tests
import qualified Day16Tests
import qualified Day17Tests
import qualified Day18Tests
import qualified Day19Tests
import qualified Day1Tests
import qualified Day20Tests
import qualified Day21Tests
import qualified Day22Tests
import qualified Day23Tests
import qualified Day24Tests
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
  testGroup "day12" Day12Tests.tests,
  testGroup "day13" Day13Tests.tests,
  testGroup "day14" Day14Tests.tests,
  testGroup "day15" Day15Tests.tests,
  testGroup "day16" Day16Tests.tests,
  testGroup "day17" Day17Tests.tests,
  testGroup "day18" Day18Tests.tests,
  testGroup "day19" Day19Tests.tests,
  testGroup "day20" Day20Tests.tests,
  testGroup "day21" Day21Tests.tests,
  testGroup "day22" Day22Tests.tests,
  testGroup "day23" Day23Tests.tests,
  testGroup "day24" Day24Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
