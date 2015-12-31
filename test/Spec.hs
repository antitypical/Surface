import Data.Name.Internal
import Surface
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "lambda" $ do
    it "produces binding abstractons" $
      lambda (expression $ Type 0) id `shouldBe` Term mempty (Expression $ Lambda (Term mempty $ Expression (Type 0)) (Term mempty $ Abstraction (Local 0) $ Term (Set.singleton (Local 0)) (Variable $ Local 0)))

  describe "show" $ do
    it "shows Type 0 without subscript" $
      show _type' `shouldBe` "Type"

    it "shows Type (n > 0) with subscript" $
      show (_type 1) `shouldBe` "Type₁"

    it "shows Type (n > 9) with subscript digits" $
      show (_type 10) `shouldBe` "Type₁₀"

    it "shows Type (n > 10) with subscript digits" $
      show (_type 12) `shouldBe` "Type₁₂"

    it "shows local variables as letters" $
      show (variable (Local 0) :: Term Expression) `shouldBe` "a"

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

identity :: Term Expression
identity = lambda _type' $ \ t -> lambda t id

constant :: Term Expression
constant = lambda _type' $ \ a -> lambda _type' $ \ b -> lambda a $ \ a' -> lambda b $ const a'
