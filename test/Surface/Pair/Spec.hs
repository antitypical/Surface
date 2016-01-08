module Surface.Pair.Spec where

import Prelude hiding (pi)
import qualified Data.Either as Either
import Surface
import Surface.Pair
import Test.Hspec

spec :: Spec
spec = do
  describe "Pair" $ do
    it "has an inferable type" $
      inferTypeOf (getValue _Pair) mempty `shouldSatisfy` Either.isRight

  where _Pair = _module ! "Pair"
