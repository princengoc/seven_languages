module Main (main) where

import System.Random (uniformR, mkStdGen)

main :: IO ()
main = do
    let gen = mkStdGen 420  -- Create a random generator using seed 42
    let (result, _) = uniformR (1, 6) gen  -- Generate a random number in the range (1,6)
    putStrLn $ show (result :: Int)  -- Print the result
