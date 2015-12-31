import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "whatever" $ do
    it "whatever" $
      True `shouldBe` True
