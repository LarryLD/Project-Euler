-- Dong, Larry
-- Last edited: Wednesday, August 30, 2017.
-- Project Euler Problem 3

-- Finished

import Data.List
import Prelude

-- number to be used on is 600851475143

answer :: Int -> Int
answer n = maximum $ is_any_prime $ list_cand ++ (find_other_half n list_cand)
    where
        list_cand = gen_cand n

gen_cand :: Int -> [Int]
gen_cand n = [q | q <- [2.. ceiling $ sqrt $ fromIntegral n], n `mod` q == 0]

find_other_half :: Int -> [Int] -> [Int]
find_other_half n list_cand = map (n `div`) list_cand -- should yield no remainder for all divisions

is_prime :: Int -> [Int] -> Bool
is_prime n [] = True
is_prime n (x:xs)
    | n `mod` x == 0 = False
    | otherwise      = is_prime n $ filter (\r -> r `mod` x /= 0) xs

is_any_prime :: [Int] -> [Int]
is_any_prime [] = []
is_any_prime (x:xs)
    | is_prime x [2.. floor $ sqrt $ fromIntegral x] = x:(is_any_prime xs)
    | otherwise = is_any_prime xs