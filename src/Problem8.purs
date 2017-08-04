module Problem8 where

import Data.Array
import Data.Maybe
import Prelude

type Matrix = Array (Array Int)

rotateMatrix :: Matrix -> Matrix
rotateMatrix m = foldl inner [] (range 0 (cols - 1))
  where
    firstCol = fromMaybe [] (head m)
    rows = length m
    cols = length firstCol
    get :: Matrix -> Int -> Int -> Int
    get m r c = fromMaybe 0 (index (fromMaybe [] (index m r)) c)
    inner :: Matrix -> Int -> Matrix
    inner m' r = snoc m' (foldl (\m'' c -> snoc m'' (get m c r)) [] (range 0 (rows - 1)))

sum :: Array Int -> Int
sum = foldl (\a b -> a + b) 0

f :: Matrix -> Int
f = rotateMatrix
  >>> map (takeWhile \n -> n /= 0)
  >>> map sum
  >>> sum

-- f [[1, 2], [0, 4], [5, 0]]
-- -> 7
