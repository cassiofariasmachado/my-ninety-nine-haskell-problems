module Lists
  ( myLast,
    myButLast,
    elementAt,
    myLength,
    myReverse,
    isPalindrome,
    flatten,
    NestedList (..),
    compress,
    pack,
    encode,
  )
where

-- Problem 1
myLast :: [a] -> Maybe a
myLast [last] = Just last
myLast (_ : tail) = myLast tail
myLast [] = Nothing

-- Problem 2
myButLast :: [a] -> Maybe a
myButLast (x : []) = Nothing
myButLast (butLast : _ : []) = Just butLast
myButLast (_ : tail) = myButLast tail
myButLast [] = Nothing

-- Problem 3

-- With !!
-- elementAt :: [a] -> Int -> Maybe a
-- elementAt [] _ = Nothing
-- elementAt list index =
--   if index - 1 >= length list || index - 1 < 0
--     then Nothing
--     else Just (list !! (index - 1))

-- Without !! (recursive)
elementAt :: [a] -> Int -> Maybe a
elementAt (head : _) 1 = Just head
elementAt [] _ = Nothing
elementAt (_ : tail) n
  | n < 1 = Nothing
  | otherwise = elementAt tail (n - 1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength [_] = 1
myLength (head : rest) = (myLength [head]) + (myLength rest)

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (head : rest) = (myReverse rest) ++ [head]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = False
isPalindrome [head] = True
isPalindrome list = (reverse list) == list

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x : xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x : tail)
  | null tail = [x]
  | x == (head tail) = compress tail
  | otherwise = x : compress tail

-- Problem 9 - revise
pack :: Eq a => [a] -> [[a]]
pack (x : xs) =
  let (first, rest) = span (== x) xs
   in (x : first) : pack rest
pack [] = []

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack