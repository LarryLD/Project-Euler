-- Dong, Larry
-- Last edited: Tuesday, August 29, 2017.
-- Project Euler Problem 1

-- Finished

import Data.List

answer :: Int -> Int
answer i = sum $ mult_3_5 (i-1)

mult_3_5 :: Int -> [Int]
mult_3_5 lim = [i | i <- [1..lim], i `mod` 5 == 0 || i `mod` 3 == 0]