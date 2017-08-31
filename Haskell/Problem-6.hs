-- Dong, Larry
-- Last edited: Wednesday, August 30, 2017.
-- Project Euler Problem 6

-- Finished

import Data.List
import Prelude

answer :: [Int] -> Int
answer nums = (sum nums)^2 - (sum $ map (^2) nums)