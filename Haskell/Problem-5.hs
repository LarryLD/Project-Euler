-- Dong, Larry
-- Last edited: Wednesday, August 30, 2017.
-- Project Euler Problem 5

-- Finished

import Data.List
import Prelude

answer :: Int -> Int
answer n = check_if_div 1 [2..n]

check_if_div :: Int -> [Int] -> Int
check_if_div acc []    = acc
check_if_div acc (x:xs)
    | x == 1    = check_if_div acc xs
    | otherwise = check_if_div (acc*x) $ map (filter_mult x) xs

filter_mult :: Int -> Int -> Int
filter_mult q n
    | n `mod` q == 0 = n `div` q
    | otherwise      = n

-- map (filter_mult 2) [3, 4, 5, 6] = [3, 2, 5, 3]
-- for numbers n too large, this algorithm fails at some point

