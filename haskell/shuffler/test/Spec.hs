module Main where

import Test.HUnit
import TestDeck (tests)

main :: IO ()
main = do
  -- Run HUnit tests from both modules
  _ <- runTestTT tests
  return ()
