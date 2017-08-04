module Problem1 where

import Prelude
import Data.List

isIncreasing :: List Int -> Boolean
isIncreasing Nil = true
isIncreasing (_ : Nil) = true
isIncreasing (x : y : ys) | x < y = isIncreasing (y : ys)
                          | otherwise = false

isPossibleIncreasing :: List Int -> Boolean
isPossibleIncreasing l = inner 0 l
  where
    inner :: Int -> List Int -> Boolean
    inner _ Nil = true
    inner _ (_ : Nil) = true
    inner n (x : y : ys) | x < y = inner x (y : ys)
                         | x > n = isIncreasing(x : ys)
                         | y > n = isIncreasing(y : ys)
                         | otherwise = false

-- isIncreasing (1 : 2 : 3 : Nil)
-- -> true

-- isIncreasing (1 : 2 : 1 : Nil)
-- -> false

-- isPossibleIncreasing (1 : 2 : 3 : 1 : 3 : 8 : Nil)
-- -> false

-- isPossibleIncreasing (1 : 2 : 3 : 1 : 5 : 8 : Nil)
-- -> true
