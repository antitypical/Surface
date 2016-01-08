module Surface.Pair.Spec where

import Prelude hiding (pi)
import qualified Data.Either as Either
import Data.Term
import Surface.Pair
import Test.Hspec

spec :: Spec
spec = do
  describe "_Pair" $ do
    it "should have an inferable type" $
      inferTypeOf _Pair mempty `shouldSatisfy` Either.isRight
