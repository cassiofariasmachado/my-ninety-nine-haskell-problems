module ListsSpec where

import Lists
  ( NestedList (..),
    compress,
    elementAt,
    encode,
    flatten,
    isPalindrome,
    myButLast,
    myLast,
    myLength,
    myReverse,
    pack,
  )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "myLast" $ do
    it "returns the last element of a list" $ do
      myLast [1, 2, 3] `shouldBe` (Just 3 :: Maybe Int)

    it "returns nothing when list is empty" $ do
      myLast [] `shouldBe` (Nothing :: Maybe Int)

  describe "myButLast" $ do
    it "returns the but last element of a list" $ do
      myButLast [1, 2, 3] `shouldBe` (Just 2 :: Maybe Int)

    it "returns head when list contains just two elements" $ do
      myButLast [1, 2] `shouldBe` (Just 1 :: Maybe Int)

    it "returns nothing when list contains just one element" $ do
      myButLast [2] `shouldBe` (Nothing :: Maybe Int)

    it "returns nothing when list is empty" $ do
      myButLast [] `shouldBe` (Nothing :: Maybe Int)

  describe "elementAt" $ do
    it "returns the first element of a list" $ do
      [1, 2, 3] `elementAt` 1 `shouldBe` (Just 1 :: Maybe Int)

    it "returns the nth element of a list" $ do
      "haskel" `elementAt` 2 `shouldBe` (Just 'a' :: Maybe Char)

    it "returns the last element of a list" $ do
      [1, 2, 3] `elementAt` 3 `shouldBe` (Just 3 :: Maybe Int)

    it "returns nothing when index is less than zero" $ do
      [1] `elementAt` 0 `shouldBe` (Nothing :: Maybe Int)

    it "returns nothing when index is equal than the lenght of list" $ do
      [1, 2, 3] `elementAt` 4 `shouldBe` (Nothing :: Maybe Int)

    it "returns nothing when index is greater than the lenght of list" $ do
      [1, 2, 3] `elementAt` 5 `shouldBe` (Nothing :: Maybe Int)

    it "returns nothing when list is empty" $ do
      [] `elementAt` 1 `shouldBe` (Nothing :: Maybe Int)

  describe "myLength" $ do
    it "returns zero when list is empty" $ do
      myLength [] `shouldBe` 0

    it "returns one when list has exactly one element" $ do
      myLength [1] `shouldBe` 1

    it "returns the lenght of a list" $ do
      myLength [1, 2] `shouldBe` 2

  describe "myReverse" $ do
    it "returns reversed list" $ do
      myReverse [2, 1] `shouldBe` [1, 2]

    it "returns reversed string" $ do
      myReverse "leksah" `shouldBe` "haskel"

    it "returns an empty list when list is empty" $ do
      myReverse [] `shouldBe` ([] :: [Int])

  describe "isPalindrome" $ do
    it "returns true when list has exactly one element" $ do
      isPalindrome [1] `shouldBe` True

    it "returns true when list is palindrome" $ do
      isPalindrome [1, 3, 1] `shouldBe` True

    it "returns false when list is not palindrome" $ do
      isPalindrome [1, 2, 3] `shouldBe` False

    it "returns true when string is palindrome" $ do
      isPalindrome "madamimadam" `shouldBe` True

    it "returns false when string is not palindrome" $ do
      isPalindrome "apple" `shouldBe` False

    it "returns false when list is empty" $ do
      isPalindrome ([] :: [Int]) `shouldBe` False

  describe "flatten" $ do
    it "returns flatted elem" $ do
      flatten (Elem 1) `shouldBe` [1]

    it "returns flatted list" $ do
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1, 2, 3, 4, 5]

  describe "compress" $ do
    it "should remove duplications" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

    it "should remove duplications when it has only duplications" $ do
      compress [2, 2, 2] `shouldBe` [2]

    it "returns an empty list when list is empty" $ do
      compress [] `shouldBe` ([] :: [Int])

  describe "pack" $ do
    it "should pack duplications in sublists" $ do
      pack "aaaabccaadeeee" `shouldBe` ["aaaa", "b", "cc", "aa", "d", "eeee"]

    it "returns an empty list when list is empty" $ do
      pack [] `shouldBe` ([] :: [[Int]])

  describe "encode" $ do
    it "returns an length encoded list" $ do
      encode "aaaabccaadeeee" `shouldBe` [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')]

    it "returns an empty list when list is empty" $ do
      encode [] `shouldBe` ([] :: [(Int, Int)])