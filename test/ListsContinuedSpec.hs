module ListsContinuedSpec where

import ListsContinued
  ( EncodedData (..),
    decodeModified,
    dropEvery,
    dupli,
    encodeDirect,
    encodeModified,
    removeAt,
    repli,
    rotate,
    slice,
    split,
  )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "encodeModified" $ do
    it "returns encoded list" $ do
      encodeModified "aaaabccaadeeee"
        `shouldBe` [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

    it "returns an empty list when list is empty" $ do
      encodeModified [] `shouldBe` ([] :: [EncodedData Int])

  describe "decodeModified" $ do
    it "returns decoded list" $ do
      decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
        `shouldBe` "aaaabccaadeeee"

    it "returns an empty list when list is empty" $ do
      decodeModified [] `shouldBe` ([] :: [Int])

  describe "encodeDirect" $ do
    it "returns encoded list" $ do
      encodeDirect "aaaabccaadeeee"
        `shouldBe` [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

    it "returns an empty list when list is empty" $ do
      encodeDirect [] `shouldBe` ([] :: [EncodedData Int])

  describe "dupli" $ do
    it "duplicate list with one element" $ do
      dupli "a" `shouldBe` "aa"

    it "duplicate list with more than one element" $ do
      dupli [1, 2, 3] `shouldBe` [1, 1, 2, 2, 3, 3]

    it "returns an empty list when list is empty" $ do
      dupli [] `shouldBe` ([] :: [Int])
  describe "repli" $ do
    it "quadruplicate list with one element" $ do
      repli "a" 4 `shouldBe` "aaaa"

    it "triplicate list with more than one element" $ do
      repli [1, 2, 3] 3 `shouldBe` [1, 1, 1, 2, 2, 2, 3, 3, 3]

    it "returns an empty list when list is empty" $ do
      repli [] 2 `shouldBe` ([] :: [Int])

  describe "dropEvery" $ do
    it "drop every second element" $ do
      dropEvery "abcdefghik" 2 `shouldBe` "acegi"

    it "drop every third element" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

    it "returns an empty list when list is empty" $ do
      dropEvery [] 3 `shouldBe` ([] :: [Int])

  describe "split" $ do
    it "split a list at third element" $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

    it "returns an tuple of empty lists when list is empty" $ do
      split [] 3 `shouldBe` (([], []) :: ([Int], [Int]))

  describe "slice" $ do
    it "slice a list from third to seventh element" $ do
      slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7 `shouldBe` "cdefg"

    it "returns an empty list when list is empty" $ do
      slice [] 3 2 `shouldBe` ([] :: [Int])

  describe "rotate" $ do
    it "rotate the list two places to left" $ do
      rotate ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] 3 `shouldBe` "defghabc"

    it "returns an empty list when list is empty" $ do
      rotate [] 2 `shouldBe` ([] :: [Int])

  describe "removeAt" $ do
    it "remove the element at position two" $ do
      removeAt 2 "abcd" `shouldBe` (Just 'b', "acd")

    it "returns an empty list when list is empty" $ do
      removeAt 2 [] `shouldBe` ((Nothing, []) :: (Maybe Int, [Int]))
