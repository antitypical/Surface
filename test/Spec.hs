import Surface
import qualified Data.Name.Internal.Spec
import qualified Data.Term.Spec
import Test.Hspec
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "Data.Name.Internal" Data.Name.Internal.Spec.spec
  describe "Data.Term" Data.Term.Spec.spec

  describe "lambda" $ do
    it "produces binding abstractions" $
      lambda _type' id `shouldBe` Term mempty (const $ Right implicit) (Binding $ Expression $ Lambda (Term mempty (const $ Right implicit) $ Type 0) (Term mempty (const $ Right implicit) $ Binding $ Abstraction (Local 0) $ Term (Set.singleton (Local 0)) (const $ Right implicit) (Binding $ Variable $ Local 0)))

    it "rejects non-Type types" $
      typeOf (lambda _type' $ \ a -> lambda a $ \ a' -> lambda a' $ const _type') mempty `shouldBe` Left ""
