import Surface
import qualified Data.Name.Internal.Spec
import qualified Data.Term.Spec
import qualified Data.Either as Either
import Test.Hspec
import Test.HUnit.Base

main :: IO ()
main = hspec $ do
  describe "Data.Name.Internal" Data.Name.Internal.Spec.spec
  describe "Data.Term" Data.Term.Spec.spec

  describe "lambda" $ do
    it "produces binding abstractions" $
      lambda _type' id `shouldBe` Term mempty (const $ Right implicit) (Binding $ Expression $ Lambda _type' (abstraction (Local 0) $ variable (Local 0)))

    it "picks fresh names" $
      (_type' `lambda` const (_type' `lambda` const _type')) `shouldBe` Term mempty (const $ Right implicit) (Binding $ Expression $ Lambda _type' $ abstraction (Local 1) $ Term mempty (const $ Right implicit) $ Binding $ Expression $ Lambda _type' $ abstraction (Local 0) _type')

    it "rejects non-Type types" $
      typeOf (lambda _type' $ \ a -> lambda a $ \ a' -> lambda a' $ const _type') mempty `shouldSatisfy` Either.isLeft

  describe "apply" $ do
    it "rejects non-function operators" $
      typeOf (apply _type' _type') mempty `shouldSatisfy` Either.isLeft

    it "typechecks as its operator’s return type" $
      typeOf (apply (_type' `lambda` \ t -> t `lambda` id) _type') mempty `shouldResult` (_type 1 --> _type 1)

  describe "-->" $ do
    it "rejects non-Type parameter types" $
      typeOf (lambda _type' $ \ a -> lambda a $ \ a' -> a' --> a) mempty `shouldSatisfy` Either.isLeft

    it "rejects non-Type return types" $
      typeOf (lambda _type' $ \ a -> lambda a $ \ a' -> a --> a') mempty `shouldSatisfy` Either.isLeft

    it "associates to the right" $
      _type' --> _type' --> _type' `shouldBe` _type' --> (_type' --> _type')

    it "is not associative" $
      (_type' --> _type') --> _type' `shouldNotBe` _type' --> (_type' --> _type')

shouldResult :: (Show a, Eq a) => Result a -> a -> Expectation
action `shouldResult` expected = either assertFailure (`shouldBe` expected) action
