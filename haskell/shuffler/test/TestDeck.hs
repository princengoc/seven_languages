module TestDeck (tests) where

-- Unit test and property testing for Deck.hs
import Test.HUnit
import Test.QuickCheck
import Deck

-- A function to filter for hearts
filterHearts :: [Card] -> [Card]
filterHearts = filter (\card -> suit card == Hearts)

-- HUnit test: Example of filtering for Hearts
testFilterHearts :: Test
testFilterHearts = TestCase (assertEqual "Should return only hearts" 
    [Card {number = 3, suit = Hearts}, Card {number = 5, suit = Hearts}] 
    (filterHearts [Card {number = 2, suit = Diamonds}, Card {number = 3, suit = Hearts}, Card {number = 4, suit = Spades}, Card {number = 5, suit = Hearts}]))

-- QuickCheck property: All cards returned by filterHearts should have the suit Hearts
prop_FilterHearts :: [Card] -> Bool
prop_FilterHearts cards = all (\card -> suit card == Hearts) (filterHearts cards)

-- HUnit test using QuickCheck
testQuickCheck :: Test
testQuickCheck = TestCase (quickCheckResult prop_FilterHearts >>= \result -> 
  case result of
    Success {} -> return ()
    _          -> assertFailure "QuickCheck test prop_FilterHearts failed")

-- Test suite combining HUnit and QuickCheck
tests :: Test
tests = TestList 
  [ TestLabel "HUnit Test: Filter Hearts" testFilterHearts
  , TestLabel "QuickCheck Property Test: FilterHearts" testQuickCheck
  ]
