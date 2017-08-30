-- Dong, Larry
-- Last edited: Tuesday, August 29, 2017.
-- Project Euler Problem 3

-- Finished

import Data.List
import Prelude

largest_prime_factor :: Int -> Int
largest_prime_factor n = maximum $ candidates ++ (is_any_prime $ gen_cand n candidates)
    where
        candidates = find_prime_factors n [2..sqrt_n] []
        sqrt_n = ceiling $ sqrt $ fromIntegral n

find_prime_factors :: Int -> [Int] -> [Int] -> [Int]
find_prime_factors n [] []  = [n]
find_prime_factors n [] acc = acc
find_prime_factors n (x:xs) acc
    | n `mod` x == 0 = find_prime_factors (n `div` x) new_cands1 (x:acc)
    | otherwise      = find_prime_factors n new_cands2 acc
    where
        new_cands1 = filter (\r -> r `mod` x /= 0 && r <= n `div `x) xs
        new_cands2 = filter (\r -> r `mod` x /= 0) xs

gen_cand :: Int -> [Int] -> [Int]
gen_cand n []     = []
gen_cand n (x:xs) = (n `div` x):(gen_cand n xs)

is_any_prime :: [Int] -> [Int]
is_any_prime []     = []
is_any_prime (x:xs)
    | is_prime x [2.. floor $ sqrt $ fromIntegral x] = x:(is_any_prime xs)
    | otherwise = is_any_prime xs

is_prime :: Int -> [Int] -> Bool
is_prime n [] = True
is_prime n (x:xs)
    | n `mod` x == 0 = False
    | otherwise      = is_prime n $ filter (\r -> r `mod` x /= 0) xs
