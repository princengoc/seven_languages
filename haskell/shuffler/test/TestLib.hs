module TestLib (libTests, genPermutation, prop_inverseTwice, prop_moveIdentity, prop_moveInverse) where

import Test.HUnit
import Test.QuickCheck
import Lib

-- Unit tests for move function
testMove :: Test
testMove = TestList
  [ 
    "move [10, 20, 30] [2, 0, 1] == [30, 10, 20]" ~: 
      move [10, 20, 30] [2, 0, 1] ~?= [30, 10, 20],
    
    "move [1, 2, 3, 4] [3, 2, 1, 0] == [4, 3, 2, 1]" ~: 
      move [1, 2, 3, 4] [3, 2, 1, 0] ~?= [4, 3, 2, 1],

    "move [5, 6, 7, 8] [1, 0, 3, 2] == [6, 5, 8, 7]" ~: 
      move [5, 6, 7, 8] [1, 0, 3, 2] ~?= [6, 5, 8, 7]
  ]

-- Unit tests for inverse function
testInverse :: Test
testInverse = TestList
  [ 
    "inverse [3, 1, 2] == [1, 2, 0]" ~: 
      inverse [3, 1, 2] ~?= [1, 2, 0],

    "inverse [1, 0, 3, 2] == [1, 0, 3, 2]" ~: 
      inverse [1, 0, 3, 2] ~?= [1, 0, 3, 2],

    "inverse [2, 3, 0, 1] == [2, 3, 0, 1]" ~: 
      inverse [2, 3, 0, 1] ~?= [2, 3, 0, 1]
  ]

-- Property-based tests using QuickCheck

-- Generate a valid permutation of length n
genPermutation :: Int -> Gen [Int]
genPermutation n = shuffle [0..n-1]

-- Property: Applying `inverse` twice should give back the original permutation
prop_inverseTwice :: Property
prop_inverseTwice = forAll (sized genPermutation) $ \sigma ->
  inverse (inverse sigma) == sigma

-- Property: Move a list with its identity permutation, should return the list itself.
prop_moveIdentity :: [Int] -> Bool
prop_moveIdentity state = move state [0..length state - 1] == state

-- Property: Moving with the inverse of a permutation should undo the permutation.
prop_moveInverse :: [Int] -> Property
prop_moveInverse state = forAll (genPermutation (length state)) $ \sigma ->
  move (move state sigma) (inverse sigma) == state

-- Combine all tests
libTests :: Test
libTests = TestList [testMove, testInverse]
