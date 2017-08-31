-- Dong, Larry
-- Last edited: Wednesday, August 30, 2017.
-- Project Euler Problem 4

-- Finished

import Data.List
import Prelude

-- upper limit is ceiling $ sqrt $ fromIntegral n
-- lower limit is n `div` 999

answer :: Int -> Int -- n is upper limit for answer, which is 999999
answer n
    | check_div n == [] = answer $ next_palin $ show n
    | otherwise         = n


check_div :: Int -> [Int]
check_div n = [q | q <- [lower..upper], n `mod` q == 0]
    where
        lower = n `div` 999
        upper = ceiling $ sqrt $ fromIntegral n

next_palin :: String -> Int -- only works for 6 digit, where a1 is in [1..9] and a2, a3, a4, a5, a6 are in [0..9]
next_palin (a1:a2:a3:as)
    | a3 == '0' && a2 == '0' = read (new_a1:'9':'9':'9':'9':new_a1:[]) :: Int
    | a3 == '0' = read (a1:new_a2:'9':'9':new_a2:a1:[]) :: Int
    | otherwise = read (a1:a2:new_a3:new_a3:a2:a1:[]) :: Int
    where
        new_a1 = next_char a1
        new_a2 = next_char a2
        new_a3 = next_char a3

next_char :: Char -> Char
next_char chr =  head $ show $ (read [chr] :: Int) - 1