module Deck (Card(..), Suit(..), mkDeckMap) where

import Data.Map (Map, fromList)
import Test.QuickCheck (Arbitrary(..), elements)

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Enum, Eq)

data Card = Card
    { number     :: Int
    , suit       :: Suit
    } deriving (Show, Eq)


-- A deck is a map from (integers) to (cards)
mkDeckMap :: Int -> Map Int Card
mkDeckMap n = fromList [(i, Card (i `mod` 13 + 1) (toEnum (i `mod` 4))) | i <- [0..(n-1)]]


-- for QuickCheck unit tests
-- Arbitrary instance for Suit
instance Arbitrary Suit where
  arbitrary = elements [Hearts, Diamonds, Clubs, Spades]

-- Arbitrary instance for Card (if needed)
instance Arbitrary Card where
  arbitrary = do
    num <- arbitrary
    s <- arbitrary
    return (Card num s)
