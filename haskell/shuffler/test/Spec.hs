module Main where

import Test.HUnit
import Test.QuickCheck
import TestDeck (tests)
import TestLib (libTests, genPermutation, prop_inverseTwice, prop_moveIdentity, prop_moveInverse)

main :: IO ()
main = do
  -- Run HUnit tests from both modules
  _ <- runTestTT tests
  _ <- runTestTT libTests
  
  -- Run QuickCheck properties
  quickCheck prop_inverseTwice
  quickCheck prop_moveIdentity
  quickCheck prop_moveInverse

  return ()
