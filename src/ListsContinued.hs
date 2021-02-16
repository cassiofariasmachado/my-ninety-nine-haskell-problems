module ListsContinued
  ( encodeModified,
    EncodedData (..),
    decodeModified,
    encodeDirect,
    dupli,
    repli,
    dropEvery,
    split,
    slice,
    rotate,
    removeAt,
  )
where

import Data.List (group)
import Lists (elementAt)

data EncodedData a = Single a | Multiple Int a
  deriving (Show)

instance (Eq a) => Eq (EncodedData a) where
  (Single a1) == (Single a2) = a1 == a2
  (Multiple n1 a1) == (Multiple n2 a2) =
    a1 == a2 && n1 == n2
  _ == _ = False

-- Problem 11
encode :: [a] -> EncodedData a
encode [last] = Single last
encode list = Multiple (length list) (head list)

encodeModified :: Eq a => [a] -> [EncodedData a]
encodeModified = map (\x -> encode x) . group

-- Problem 12
decode :: EncodedData a -> [a]
decode (Multiple n a) = take n (repeat a)
decode (Single a) = [a]

decodeModified :: Eq a => [EncodedData a] -> [a]
decodeModified = concatMap (\x -> decode x)

-- Problem 13 - Copied
encode' :: Eq a => [a] -> [(Int, a)]
encode' = foldr helper []
  where
    helper x [] = [(1, x)]
    helper x (head@(left, right) : tail)
      | x == right = (1 + left, x) : tail
      | otherwise = (1, x) : head : tail

encodeDirect :: Eq a => [a] -> [EncodedData a]
encodeDirect = map encodeHelper . encode'
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli [x] = x : [x]
dupli (head : tail) = (dupli [head]) ++ (dupli tail)

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] n = []
repli [head] n = [head | _ <- [1 .. n]]
repli (head : tail) n = (repli [head] n) ++ (repli tail n)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery list n = [elem | (elem, index) <- zip list [1 ..], index `mod` n /= 0]

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split [] n = ([], [])
split list n = (take n list, drop n list)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice list x y = take (y - x + 1) (drop (x - 1) list)

-- Problem 19 - Copied from https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: [a] -> Int -> [a]
rotate [] n = []
rotate list n = zipWith const (drop n (cycle list)) list

-- Problem 20
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n [] = (Nothing, [])
removeAt n list = (elementAt list n, [elem | (elem, index) <- zip list [1 ..], index /= n])