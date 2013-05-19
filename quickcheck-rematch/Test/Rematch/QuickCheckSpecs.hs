module Test.Rematch.QuickCheckSpecs where
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Control.Rematch
import Test.Rematch.QuickCheck

main :: IO ()
main = hspec $ specs

specs :: Spec
specs = do
  prop "it can be used for quickcheck tests" $
    (\i -> expectP i (greaterThanOrEqual (minBound :: Int)))
