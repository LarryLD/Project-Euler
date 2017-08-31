-- Dong, Larry
-- Last edited: Wednesday, August 30, 2017.
-- Project Euler Problem 10

-- Finished, but slow, 29.31 secs

import Data.List
import Prelude

answer :: Int -> Int
answer n = gen_primes 0 [2] [3..n]


gen_primes :: Int -> [Int] -> [Int] -> Int
gen_primes acc primes []   = acc + sum primes
gen_primes acc (x:xs) cand = gen_primes (acc+x) new_xs trunc_cand
    where
        new_xs     = xs ++ filter (< x^2) new_cand
        trunc_cand = filter (> x^2) new_cand
        new_cand   = filter (\r -> r `mod` x /= 0) cand