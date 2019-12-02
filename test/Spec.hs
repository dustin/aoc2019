{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty

import qualified ComputerTests
import qualified Day1Tests
import qualified Day2Tests
import qualified SearchTests

tests :: [TestTree]
tests = [
  testGroup "search" SearchTests.tests,
  testGroup "computer" ComputerTests.tests,
  testGroup "day 1" Day1Tests.tests,
  testGroup "day 2" Day2Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
