import Data.Name.Internal
import Surface
import qualified NameSpec
import qualified Data.Term.ShowSpec
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "Name" NameSpec.spec
  describe "Data.Term.Show" Data.Term.ShowSpec.spec

  describe "lambda" $ do
    it "produces binding abstractons" $
      lambda (typing $ Type 0) id `shouldBe` Term mempty Nothing (Binding $ Expression $ Lambda (Term mempty Nothing $ Type 0) (Term mempty Nothing $ Binding $ Abstraction (Local 0) $ Term (Set.singleton (Local 0)) Nothing (Binding $ Variable $ Local 0)))

identity :: Term Expression
identity = lambda _type' $ \ t -> lambda t id

constant :: Term Expression
constant = lambda _type' $ \ a -> lambda _type' $ \ b -> lambda a $ \ a' -> lambda b $ const a'
