module Shuffle where
    
import Data.List (sortBy)
import Data.Function (on)        
import Lib (inverse)

data InverseRiffle = InverseRiffle
data Riffle = Riffle

class Shuffle s where
    -- generate a shuffle step from a sequence of realized random variables
    shuffle :: s -> [Int] -> [Int]

instance Shuffle InverseRiffle where
    -- Label the n cards with n independent fairly chosen bits. Pull all the cards
    -- labeled 0 to the top of the deck, preserving their relative order.
    shuffle InverseRiffle xs = map fst sortedTuple
        where
            n = length xs
            tuple = zip [0..(n-1)] xs
            sortedTuple = sortBy (compare `on` snd) tuple

instance Shuffle Riffle where
    shuffle Riffle xs = inverse (shuffle InverseRiffle xs)

