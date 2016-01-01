import Surface
import qualified Data.Name.Internal.Spec
import qualified Data.Term.Spec
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "Data.Name.Internal" Data.Name.Internal.Spec.spec
  describe "Data.Term" Data.Term.Spec.spec

  describe "lambda" $ do
    it "produces binding abstractions" $
      lambda (typing $ Type 0) id `shouldBe` Term mempty (Right implicit) (Binding $ Expression $ Lambda (Term mempty (Right implicit) $ Type 0) (Term mempty (Right implicit) $ Binding $ Abstraction (Local 0) $ Term (Set.singleton (Local 0)) (Right implicit) (Binding $ Variable $ Local 0)))

identity :: Term Expression
identity = lambda _type' $ \ t -> lambda t id

constant :: Term Expression
constant = lambda _type' $ \ a -> lambda _type' $ \ b -> lambda a $ \ a' -> lambda b $ const a'
