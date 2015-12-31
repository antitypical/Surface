import Surface
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "lambda" $ do
    it "produces binding abstractons" $
      lambda (expression $ Type 0) id `shouldBe` Term mempty (Expression $ Lambda (Term mempty $ Expression (Type 0)) (Term mempty $ Abstraction (Local 0) $ Term (Set.singleton (Local 0)) (Variable $ Local 0)))

identity :: Term Expression
identity = lambda _type' $ \ t -> lambda t id
