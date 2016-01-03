{-# LANGUAGE ImplicitParams #-}
module Test.Assertions (
  shouldResult,
) where

import Surface
import Test.Hspec
import Test.HUnit.Base
import GHC.Stack

infixl 1 `shouldResult`

shouldResult :: (?loc :: CallStack, Show a, Eq a) => Result a -> a -> Expectation
action `shouldResult` expected = either assertFailure (`shouldBe` expected) action
