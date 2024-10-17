module Main (main) where

--import System.Random (uniformR, mkStdGen)

import Prelude 
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler.Strict

model :: Distribution Double
model = do
    x <- bernoulli 0.5
    normal (if x then (-3) else 3) 1

-- get statistics on a given sample
getMean :: [Double] -> Double
getMean x = (sum x) / fromIntegral (length x)

main :: IO ()
main = do
    --let gen = mkStdGen 420  -- Create a random generator using seed 42
    --let (result, _) = uniformR (1, 6) gen  -- Generate a random number in the range (1,6)
    -- putStrLn $ show (result :: Int)  -- Print the result

    -- use relicateM
    result <- (sampler . replicateM 10) model -- unwrap the sample
    print $ getMean result
    

