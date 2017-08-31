-- Dong, Larry
-- Last edited: Tuesday, August 29, 2017.
-- Project Euler Problem 2

-- Finished

import Data.List

answer :: Int -> Int
answer lim = sum $ gen_even_fibs lim (2:1:[])

gen_even_fibs :: Int -> [Int] -> [Int]
gen_even_fibs lim (t2:t1:ts)
    | t2 < lim = gen_even_fibs lim ((t2 + t1):t2:t1:ts)
    | otherwise = filter (\r -> r `mod` 2 == 0) (t2:t1:ts)
-- limit is upper limit, t2 is second term in the Fib sequence and t1 is first