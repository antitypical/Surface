module Test.Assertions where

import Surface
import Test.Hspec
import Test.HUnit.Base

shouldResult :: (Show a, Eq a) => Result a -> a -> Expectation
action `shouldResult` expected = either assertFailure (`shouldBe` expected) action
