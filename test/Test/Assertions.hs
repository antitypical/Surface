module Test.Assertions (
  shouldResult,
) where

import Surface
import Test.Hspec
import Test.HUnit.Base

infixl 1 `shouldResult`

shouldResult :: (Show a, Eq a) => Result a -> a -> Expectation
action `shouldResult` expected = either assertFailure (`shouldBe` expected) action
