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

identity :: Term Expression
identity = lambda _type' $ \ t -> lambda t id

constant :: Term Expression
constant = lambda _type' $ \ a -> lambda _type' $ \ b -> lambda a $ \ a' -> lambda b $ const a'
