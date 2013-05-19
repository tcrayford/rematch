module Control.Rematch.HashSet.Specs where
import Control.Rematch(Matcher, match, description, describeMismatch)
import Control.Rematch.HashSet
import Test.Hspec
import Test.HUnit
import Test.Hspec.HUnit()
import qualified Data.HashSet as H

main :: IO ()
main = hspec $ specs

specs :: Spec
specs = describe "rematch-unordered-containers" $ do
  describe "HashSet" $ do
    describe "isEmpty" $ do
      it "passes if the input set is empty" $
        checkMatch (fromList []) isEmpty @?= Nothing

      it "fails if the input set is not empty" $
        checkMatch (fromList [1]) isEmpty @?= Just ("isEmpty","was fromList [1]")

    describe "hasSize" $ do
      it "passes if the input set has the right size" $
        checkMatch (fromList []) (hasSize 0) @?= Nothing

      it "fails if the input set has the wrong size" $
        checkMatch (fromList []) (hasSize 1) @?= Just ("hasSize 1","had size 0")

    describe "hasMember" $ do
      it "passes if the input set has the right member" $
        checkMatch (fromList [1]) (hasMember 1) @?= Nothing

      it "fails if the input set doesn't have the right member" $
        checkMatch (fromList [1]) (hasMember 2) @?= Just ("hasMember 2","was fromList [1]")

    describe "containsSet" $ do
      it "passes if the input set has the right member" $
        checkMatch (fromList [1]) (containsSet (fromList [1])) @?= Nothing

      it "fails if the input set doesn't have the right member" $
        checkMatch (fromList [1]) (containsSet (fromList [2])) @?= Just ("containsSet fromList [2]","was fromList [1]")

fromList :: [Int] -> H.HashSet Int
fromList = H.fromList

checkMatch :: a -> Matcher a -> Maybe (String, String)
checkMatch a m = if match m a
  then Nothing
  else Just (description m, describeMismatch m a)
