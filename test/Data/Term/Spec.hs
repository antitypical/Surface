module Data.Term.Spec (spec) where

import Prelude hiding (pi)
import Surface
import qualified Data.Map as Map
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary Name where
  arbitrary = oneof [
      Local <$> arbitrary,
      Global <$> arbitrary
    ]

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
    prop "infers the type of Type" $
      \ n -> infer (_type n) `shouldBe` Right (_type $ n + 1)

    it "infers the type of functions over types" $
      infer (_type' --> _type') `shouldBe` Right (_type' --> _type')

    it "infers the type of pi types" $
      infer (_type' `pi` id) `shouldBe` Right (_type' --> _type')

    prop "infers the types of variables bound in the context" $
      \ name -> inferBinding name _type' (variable name) `shouldBe` Right _type'

    it "infers the types of constant lambdas" $
      infer (_type' `lambda` const _type') `shouldBe` Right (_type' --> _type')

    it "infers the type of `identity`" $
      infer identity `shouldBe` Right (_type' `pi` (\ a -> a --> a))

    it "infers the type of `constant`" $
      infer constant `shouldBe` Right (_type' `pi` (\ a -> _type' `pi` (\ b -> a --> b --> a)))

  describe "substitute" $ do
    prop "is identity on Type" $
      \ name n -> substitute name (_type $ n + 1) (_type n) `shouldBe` (_type n :: Term Expression)

    prop "replaces variables with the same name" $
      \ name -> substitute name _type' (variable name) `shouldBe` (_type' :: Term Expression)

    prop "does not replace other variables" $
      \ name -> substitute name _type' (variable $ prime name) `shouldBe` (variable (prime name) :: Term Expression)

    prop "transits non-shadowing abstractions" $
      \ name -> substitute name _type' (abstraction (prime name) (variable name)) `shouldBe` (abstraction (prime name) _type' :: Term Expression)

    prop "does not replace shadowed variables" $
      \ name -> substitute name _type' (abstraction name (variable name)) `shouldBe` (abstraction name (variable name) :: Term Expression)

    prop "transits expressions" $
      \ name -> substitute name _type' (apply (variable name) (variable name)) `shouldBe` apply _type' _type'

    prop "transits annotations" $
      \ name -> substitute name _type' (annotation (variable name) (variable name)) `shouldBe` (annotation _type' _type' :: Term Expression)

  describe "rename" $ do
    prop "is identity on Type" $
      \ name n -> rename name (prime name) (_type n) `shouldBe` (_type n :: Term Expression)

    prop "replaces identical variables" $
      \ name -> rename name (prime name) (variable name) `shouldBe` (variable (prime name) :: Term Expression)

    prop "does not replace other variables" $
      \ name -> rename (prime $ prime name) (prime name) (variable name) `shouldBe` (variable name :: Term Expression)

    prop "transits non-shadowing abstractions" $
      \ name -> rename name (prime name) (abstraction (prime $ prime name) (variable name)) `shouldBe` (abstraction (prime $ prime name) (variable (prime name)) :: Term Expression)

    prop "does not replace shadowed variables" $
      \ name -> rename name (prime name) (abstraction name (variable name)) `shouldBe` (abstraction name (variable name) :: Term Expression)

    prop "transits expressions" $
      \ name -> rename name (prime name) (apply (variable name) (variable name)) `shouldBe` apply (variable $ prime name) (variable $ prime name)

    prop "transits annotations" $
      \ name -> rename name (prime name) (annotation (variable name) (variable name)) `shouldBe` (annotation (variable $ prime name) (variable $ prime name) :: Term Expression)

infer :: Term Expression -> Either String (Term Expression)
infer term = typeOf term mempty

inferBinding :: Name -> Term Expression -> Term Expression -> Either String (Term Expression)
inferBinding name type' term = typeOf term $ Map.singleton name type'

identity :: Term Expression
identity = lambda _type' $ \ t -> lambda t id

constant :: Term Expression
constant = lambda _type' $ \ a -> lambda _type' $ \ b -> lambda a $ \ a' -> lambda b $ const a'
