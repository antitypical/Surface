module Surface.Language.Spec where

import Surface
import qualified Data.Either as Either
import Test.Assertions
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "lambda" $ do
    it "produces binding abstractions" $
      lambda _type' id `shouldBe` Term mempty (typeOf implicit) (Binding $ Expression $ Lambda _type' (abstraction (Local 0) $ variable (Local 0)))

    it "picks fresh names" $
      (_type' `lambda` const (_type' `lambda` const _type')) `shouldBe` Term mempty (typeOf implicit) (Binding $ Expression $ Lambda _type' $ abstraction (Local 1) $ Term mempty (typeOf implicit) $ Binding $ Expression $ Lambda _type' $ abstraction (Local 0) _type')

    it "rejects non-Type types" $
      inferTypeOf (_type' `lambda` \ b -> b `lambda` \ a -> a `lambda` const _type') mempty `shouldSatisfy` Either.isLeft

  describe "apply" $ do
    it "rejects non-function operators" $
      inferTypeOf (apply _type' _type') mempty `shouldSatisfy` Either.isLeft

    it "typechecks as its operator’s return type" $
      inferTypeOf (apply (_type' `lambda` \ t -> t `lambda` id) _type') mempty `shouldResult` _type 0 --> _type 0

  describe "-->" $ do
    it "rejects non-Type parameter types" $
      inferTypeOf (_type' `lambda` \ a -> a `lambda` \ a' -> a' --> a) mempty `shouldSatisfy` Either.isLeft

    it "rejects non-Type return types" $
      inferTypeOf (_type' `lambda` \ a -> a `lambda` \ a' -> a --> a') mempty `shouldSatisfy` Either.isLeft

    it "associates to the right" $
      _type' --> _type' --> _type' `shouldBe` _type' --> (_type' --> _type')

    it "is not associative" $
      (_type' --> _type') --> _type' `shouldNotBe` _type' --> (_type' --> _type')

  describe "checkIsType" $ do
    prop "matches Type" $
      \ n -> checkIsType (_type n) mempty `shouldResult` _type'

    prop "matches function types" $
      \ m n -> checkIsType (_type m --> _type n) mempty `shouldResult` (_type' --> _type')