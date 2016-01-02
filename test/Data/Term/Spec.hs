module Data.Term.Spec (spec) where

import Prelude hiding (pi)
import Surface
import Test.Hspec

spec :: Spec
spec = do
  describe "show" $ do
    it "shows Type 0 without subscript" $
      show (_type' :: Term Expression) `shouldBe` "Type"

    it "shows Type (n > 0) with subscript" $
      show (_type 1 :: Term Expression) `shouldBe` "Type₁"

    it "shows Type (n > 9) with subscript digits" $
      show (_type 10 :: Term Expression) `shouldBe` "Type₁₀"

    it "shows Type (n > 10) with subscript digits" $
      show (_type 12 :: Term Expression) `shouldBe` "Type₁₂"

    it "shows local variables as letters" $
      show (variable (Local 0) :: Term Expression) `shouldBe` "a"

    it "bare applications are unparenthesized" $
      show (apply (variable (Local 0)) (variable (Local 1))) `shouldBe` "a b"

    it "associates applications to the left without parentheses" $
      show (apply (apply (variable (Local 0)) (variable (Local 1))) (variable (Local 2))) `shouldBe` "a b c"

    it "parenthesizes right-nested applications" $
      show (apply (variable (Local 0)) (apply (variable (Local 1)) (variable (Local 2)))) `shouldBe` "a (b c)"

    it "associates lambdas to the right without parentheses" $
      show identity `shouldBe` "λ b : Type . λ a : b . a"

    it "renders lambdas without abstractions as function types" $
      show (_type' --> _type') `shouldBe` "Type → Type"

    it "renders right-nested function types without parentheses" $
      show (_type' --> _type' --> _type') `shouldBe` "Type → Type → Type"

  describe "typeOf" $ do
    it "infers the type of Type" $
      infer _type' `shouldBe` Right (_type 1)

    it "infers the type of `identity`" $
      infer identity `shouldBe` Right (_type' `pi` (\ a -> a --> a))

    it "infers the type of `constant`" $
      infer constant `shouldBe` Right (_type' `pi` (\ a -> _type' `pi` (\ b -> a --> b --> a)))

infer :: Term Expression -> Either String (Term Expression)
infer term = typeOf term mempty

identity :: Term Expression
identity = lambda _type' $ \ t -> lambda t id

constant :: Term Expression
constant = lambda _type' $ \ a -> lambda _type' $ \ b -> lambda a $ \ a' -> lambda b $ const a'
