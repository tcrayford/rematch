module Test.Rematch.Specs.TastyHUnit where
import Test.Hspec
import Test.Hspec.HUnit()
import Test.Rematch.HUnit
import Control.Rematch(is)

main :: IO ()
main = hspec $ specs

specs :: Spec
specs = do
  describe "expect" $ do
    -- it isn't clear how to run a tasty test programatically and extract its result,
    -- so I'm not sure how to write a test for this.
    it "is used as a tasty-hunit test" pending

       
