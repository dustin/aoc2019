{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty

import qualified ComputerTests
import qualified Day1Tests
import qualified Day2Tests
import qualified Day3Tests
import qualified Day4Tests
import qualified Day5Tests
import qualified SearchTests

tests :: [TestTree]
tests = [
  testGroup "search" SearchTests.tests,
  testGroup "computer" ComputerTests.tests,
  testGroup "day 1" Day1Tests.tests,
  testGroup "day 2" Day2Tests.tests,
  testGroup "day 3" Day3Tests.tests,
  testGroup "day 4" Day4Tests.tests,
  testGroup "day 5" Day5Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
