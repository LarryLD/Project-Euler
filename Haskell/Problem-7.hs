-- Dong, Larry
-- Last edited: Wednesday, August 30, 2017.
-- Project Euler Problem 7

-- Finished, but slow, takes 48.39 secs

import Data.List
import Prelude

answer :: Int -> Int
answer lim = head $ find_primes lim 3 [2] -- maximum should occur at head by construction

find_primes :: Int -> Int -> [Int] -> [Int]
find_primes lim cand acc
    | length acc < lim = find_primes lim (cand+2) (gen_primes cand acc) -- save time, no other prime is even other than 2
    | otherwise        = acc

gen_primes :: Int -> [Int] -> [Int]
gen_primes n acc
    | is_prime n [i | i <- acc, i <= upper] = n:acc
    | otherwise = acc
    where
        upper = ceiling $ sqrt $ fromIntegral n

is_prime :: Int -> [Int] -> Bool
is_prime n [] = True
is_prime n (x:xs)
    | n `mod` x == 0 = False
    | otherwise      = is_prime n xs