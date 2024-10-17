module Lib (move, inverse) where

import Data.List (sortBy)
import Data.Function (on)        

-- TODO: write unit tests

-- frequency table: Data.Judy or Data.Map

-- apply a permutation sigma to a state 
-- [3,1,2] . x = [x[3], x[1], x[2]]
move :: [Int] -> [Int] -> [Int]
move state sigma = map (state !!) sigma

-- invert a permutation
-- [3,1,2] --> [2,3,1], ie move a to 3, b to 1, c to 2
inverse :: [Int] -> [Int]
inverse sigma = 
    let 
        n = length sigma
        tuple = zip [0..(n-1)] sigma
        sortedTuple = sortBy (compare `on` snd) tuple
    in
        map fst sortedTuple

