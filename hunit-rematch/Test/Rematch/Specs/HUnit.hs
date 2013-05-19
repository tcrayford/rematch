module Test.Rematch.Specs.HUnit where
import Test.Hspec
import Test.HUnit
import Test.Hspec.HUnit()
import Test.Rematch.HUnit
import Control.Rematch(is)

main :: IO ()
main = hspec $ specs

specs :: Spec
specs = do
  describe "expect" $ do
    it "is used as an hunit test" $
      expect "a" (is "a")
