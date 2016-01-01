import Data.Name.Internal
import Surface
import qualified NameSpec
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "Name" NameSpec.spec
  
  describe "lambda" $ do
    it "produces binding abstractons" $
      lambda (typing $ Type 0) id `shouldBe` Term mempty Nothing (Binding $ Expression $ Lambda (Term mempty Nothing $ Type 0) (Term mempty Nothing $ Binding $ Abstraction (Local 0) $ Term (Set.singleton (Local 0)) Nothing (Binding $ Variable $ Local 0)))

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

identity :: Term Expression
identity = lambda _type' $ \ t -> lambda t id

constant :: Term Expression
constant = lambda _type' $ \ a -> lambda _type' $ \ b -> lambda a $ \ a' -> lambda b $ const a'
