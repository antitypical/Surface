import qualified Data.Name.Internal.Spec
import qualified Data.Term.Spec
import qualified Surface.Language.Spec
import qualified Surface.Pair.Spec
import Test.Hspec


main :: IO ()
main = hspec . parallel $ do
  describe "Data.Name.Internal" Data.Name.Internal.Spec.spec
  describe "Data.Term" Data.Term.Spec.spec
  describe "Surface.Language" Surface.Language.Spec.spec
  describe "Surface.Pair" Surface.Pair.Spec.spec
