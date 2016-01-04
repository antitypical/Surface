{-# LANGUAGE FlexibleInstances #-}
module Data.Term.Spec (spec) where

import Prelude hiding (pi)
import Surface
import qualified Data.Map as Map
import Test.Assertions
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance Arbitrary Name where
  arbitrary = oneof [
      Local <$> arbitrary,
      Global <$> arbitrary
    ]

instance Arbitrary (Term Expression) where
  arbitrary = arbitraryInScope []
    where arbitraryInScope names = frequency $
            (16, _type <$> arbitrary)
            : (4, pure implicit)
            : (1, annotation <$> recur names <*> recur names)
            : (1, arbitrary >>= \ name -> abstraction name <$> recur (name : names))
            : (1, expression <$> (Lambda <$> recur names <*> recur names))
            : (1, expression <$> (Application <$> recur names <*> recur names))
            : ((,) 4 . pure . variable <$> names)
          recur names = scale (`div` 2) (arbitraryInScope names)
          expression = checkedExpression (checkInferred (inferSpecific implicit))

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

    it "renders left-nested function types with parentheses" $
      show ((_type' --> _type') --> _type') `shouldBe` "(Type → Type) → Type"

  describe "typeOf" $ do
    prop "infers the type of Type" $
      \ n -> infer (_type n) `shouldResult` _type (n + 1)

    it "infers the type of functions over types" $
      infer (_type' --> _type') `shouldResult` _type' --> _type'

    it "infers the type of pi types" $
      infer (_type' `pi` id) `shouldResult` _type' --> _type'

    prop "infers the types of variables bound in the context" $
      \ name -> inferBinding name _type' (variable name) `shouldResult` _type'

    it "infers the types of constant lambdas" $
      infer (_type' `lambda` const _type') `shouldResult` _type' --> _type 1

    it "infers the type of `identity`" $
      infer identity `shouldResult` _type' `pi` (\ a -> a --> a)

    it "infers the type of `constant`" $
      infer constant `shouldResult` _type' `pi` (\ a -> _type' `pi` (\ b -> a --> b --> a))

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

  describe "unify" $ do
    prop "_ is left unit" $
      \ term -> unify implicit term `shouldBe` into (term :: Term Expression)

    prop "_ is right unit" $
      \ term -> unify term implicit `shouldBe` into (term :: Term Expression)

    prop "identical terms unify" $
      \ term -> unify term term `shouldBe` into (term :: Term Expression)

    prop "binding & non-binding functions unify" $
      \ n -> unify (_type n --> _type n) (_type n `pi` const (_type n)) `shouldBe` into (_type n --> _type n :: Term Expression)

    prop "equal variables unify" $
      \ name -> unify (variable name) (variable name) `shouldBe` into (variable name :: Term Expression)

    prop "abstractions over the same name unify" $
      \ name term -> unify (abstraction name implicit) (abstraction name term) `shouldBe` into (abstraction name term :: Term Expression)


infer :: Term Expression -> Either String (Term Expression)
infer = (`inferTypeOf` mempty)

inferBinding :: Name -> Term Expression -> Term Expression -> Either String (Term Expression)
inferBinding name type' term = inferTypeOf term $ Map.singleton name type'

identity :: Term Expression
identity = lambda _type' $ \ t -> lambda t id

constant :: Term Expression
constant = lambda _type' $ \ a -> lambda _type' $ \ b -> lambda a $ \ a' -> lambda b $ const a'
