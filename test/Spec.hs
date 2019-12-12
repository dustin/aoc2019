{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty

import qualified ComputerSTTests
import qualified ComputerTests
import qualified Day10Tests
import qualified Day11Tests
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
  testGroup "search" SearchTests.tests,
  testGroup "vis" VisTests.tests,
  testGroup "OK computer" OKComputerTests.tests,
  testGroup "computer" ComputerTests.tests,
  testGroup "computer ST" ComputerSTTests.tests,
  testGroup "day 1" Day1Tests.tests,
  testGroup "day 2" Day2Tests.tests,
  testGroup "day 3" Day3Tests.tests,
  testGroup "day 4" Day4Tests.tests,
  testGroup "day 5" Day5Tests.tests,
  testGroup "day 6" Day6Tests.tests,
  testGroup "day 7" Day7Tests.tests,
  testGroup "day 8" Day8Tests.tests,
  testGroup "day 9" Day9Tests.tests,
  testGroup "day 10" Day10Tests.tests,
  testGroup "day 11" Day11Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
