module SubsetSum
( subsetSumMap
, subsetSumHash
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.HashTable as Hash
import qualified Data.Maybe as Maybe

-- Given a list, return a list with every other element removed.
everyOther :: [a] -> [a]
everyOther (x:[]) = [x]
everyOther (x:xs) = x:everyOther (tail xs)
everyOther [] = []

-- Split a single list int a tuple of two lists of roughly equal length.
splitList :: [a] -> ([a], [a])
splitList xs = (everyOther xs, everyOther (tail xs))

-- After splitting the list into two and adding all sums of the first
-- half into a Map, for each entry in the second half, check if the
-- difference between its sum and the desired value is already in the
-- map.  If so, concatenate the value list store in the map with the
-- current value list, as we've found a match.
checkMap :: Map.Map Int [Int] -> Int -> [(Int, [Int])] -> [Int]
checkMap h a (x:xs) =
  let (s, ys) = x
      d = a - s
      j = Map.lookup d h
  in case j of
     Just zs -> zs ++ ys
     Nothing -> checkMap h a xs
checkMap h _ [] = []

checkHash :: Hash.HashTable Int [Int] -> Int -> [(Int, [Int])] -> IO [Int]
checkHash h a (x:xs) = do
  let (s, ys) = x
      d = a - s
  j <- Hash.lookup h d
  case j of
    Just zs -> return $ zs ++ ys
    Nothing -> checkHash h a xs
checkHash h _ [] = return []

-- |Given sum and list of values, return subset that sums to given sum,
-- using a pure map.  This uses a simple meet-in-the-middle algorithm.
-- The subset sum problem is NP-complete, and this implementation is
-- not suitable for large inputs (>30 numbers).
subsetSumMap :: Int -> [Int] -> [Int]
subsetSumMap 0 _ = []
subsetSumMap a xs =
  let (x1, x2) = splitList xs
      withSum ys = (foldr (+) 0 ys, ys)
      h = Map.fromList $ map withSum $ List.subsequences x1
  in checkMap h a $ map withSum $ List.subsequences x2

-- |Given sum and list of values, return subset that sums to given sum,
-- using in impure hash table. This uses a simple meet-in-the-middle
-- algorithm.  The subset sum problem is NP-complete, and this
-- implementation is not suitable for large inputs (>30 numbers).
subsetSumHash :: Int -> [Int] -> IO [Int]
subsetSumHash 0 _ = return []
subsetSumHash a xs = do
  let (x1, x2) = splitList xs
      withSum ys = (foldr (+) 0 ys, ys)
  h <- Hash.fromList Hash.hashInt $ map withSum $ List.subsequences x1
  checkHash h a $ map withSum $ List.subsequences x2
