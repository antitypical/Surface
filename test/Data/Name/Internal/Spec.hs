module Data.Name.Internal.Spec where

import Data.Name.Internal
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "digits" $ do
    it "zero is zero in base 10" $
      digits 10 0 `shouldBe` [0]

    it "10 has two digits in base 10" $
      digits 10 10 `shouldBe` [1, 0]

    it "1234’s digits are produced in order in base 10" $
      digits 10 1234 `shouldBe` [1, 2, 3, 4]

    it "produces two digits for 255 in base 16" $
      digits 16 255 `shouldBe` [15, 15]

  describe "showNumeral" $ do
    it "selects single letters in an alphabet" $
      showNumeral "a" 0 `shouldBe` "a"

    it "handles indices into degenerate 0-length alphabets" $
      showNumeral "" 0 `shouldBe` ""

    it "handles out-of-bounds indices into degenerate 1-length alphabets" $
      showNumeral "a" 1 `shouldBe` "a"

    it "handles out-of-bounds indices into reasonable alphabets" $
      showNumeral "ab" 2 `shouldBe` "ba"

  describe "interleave" $ do
    prop "ignores empty lists at left" $
      \ a -> "" `interleave` a `shouldBe` a

    prop "ignores empty lists at right" $
      \ a -> a `interleave` "" `shouldBe` a

    prop "is as long as both its arguments combined" $
      \ a b -> length (a `interleave` b) `shouldBe` length a + length (b :: String)

    prop "is left-biased" $
      \ a b -> ('a' : a) `interleave` b `shouldBe` 'a' : (b `interleave` a)
