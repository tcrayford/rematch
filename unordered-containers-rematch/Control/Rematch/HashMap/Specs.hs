module Control.Rematch.HashMap.Specs where
import Control.Rematch
import Test.Hspec
import Test.HUnit
import Test.Hspec.HUnit()
import qualified Data.HashMap.Strict as S
import qualified Control.Rematch.HashMap.Strict as RS

main :: IO ()
main = hspec $ specs

specs :: Spec
specs = describe "rematch-unordered-containers" $ do
  describe "HashMap.Strict" $ do
    describe "isEmpty" $ do
      it "isEmpty passes when the input map is empty" $
        checkMatch (fromList []) RS.isEmptyMap @?= Nothing

      it "isEmpty fails when the input map isn't empty" $
        checkMatch (fromList [(1,2)]) RS.isEmptyMap @?= Just ("isEmptyMap", "was fromList [(1,2)]")

    describe "hasSize" $ do
      it "hasSize passes when the size is correct" $
        checkMatch (S.fromList [("", "")]) (RS.hasSize 1) @?= Nothing

      it "hasSize fails when the size is incorrect" $
        checkMatch (fromList [(1,2)]) (RS.hasSize 0) @?= Just ("hasSize 0","was fromList [(1,2)]")

    describe "hasKey" $ do
      it "hasKey passes when the list has the right key" $
        checkMatch (fromList [(1,2)]) (RS.hasKey 1) @?= Nothing

      it "hasKey fails when the map doesn't have the key" $
        checkMatch (fromList [(1,2)]) (RS.hasKey 2) @?= Just ("hasKey 2","was fromList [(1,2)]")
    describe "hasValueAt" $ do
      it "hasValueAt passes when the specified value is found at the specified key" $
        checkMatch (fromList [(1,2)]) (RS.hasValueAt 1 2) @?= Nothing

      it "hasValueAt fails with a message about nothing if there was nothing there" $
        checkMatch (fromList [(1,2)]) (RS.hasValueAt 2 2) @?= Just ("hasValueAt 2 2","was Nothing")

      it "hasValueAt fails with a message about the wrong value" $
        checkMatch (fromList [(1,2)]) (RS.hasValueAt 1 3) @?= Just ("hasValueAt 1 3","was Just 2")

    describe "containsMap" $ do
      it "containsMap passes when both maps are empty" $
        checkMatch (fromList []) (RS.containsMap (fromList [])) @?= Nothing

      it "containsMap passes when the expected contains the incoming" $
        checkMatch (fromList [(1,2)]) (RS.containsMap (fromList [(1,2)])) @?= Nothing

      it "containsMap fails when the expected doesn't contain the incoming" $
        checkMatch (fromList [(2,2)]) (RS.containsMap (fromList [(1,2)])) @?= Just ("containsMap fromList [(1,2)]","was fromList [(2,2)]")

    describe "hasKeys" $ do
      it "passes when the map has the right keys" $ do
        checkMatch (fromList [(1,2)]) (RS.hasKeys [1]) @?= Nothing

      it "passes when the map has the right keys" $ do
        checkMatch (fromList [(1,2)]) (RS.hasKeys []) @?= Just ("hasKeys []","had keys [1]")

    describe "hasValues" $ do
      it "passes when the map has the right values" $ do
        checkMatch (fromList [(1,1)]) (RS.hasValues [1]) @?= Nothing

      it "passes when the map has the right values" $ do
        checkMatch (fromList [(1,1)]) (RS.hasValues []) @?= Just ("hasValues []","had values [1]")

fromList :: [(Int, Int)] -> S.HashMap Int Int
fromList = S.fromList

checkMatch :: a -> Matcher a -> Maybe (String, String)
checkMatch a m = if match m a
  then Nothing
  else Just (description m, describeMismatch m a)
