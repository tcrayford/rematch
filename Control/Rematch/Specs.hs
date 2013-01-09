module Control.Rematch.Specs where
import Test.Hspec
import Test.HUnit
import Test.Hspec.HUnit()
import Control.Rematch

main :: IO ()
main = hspec $ do
  describe "rematch core" $ do
    it "passes" $ do
      runMatch (hasItem 'a') "a" @?= MatchSuccess

    it "describes failure" $
      runMatch (hasItem 'a') "b" @?= MatchFailure "Expected: 'a' to be in \"b\""

    it "can be inverted" $
      runMatch (no (hasItem 'a')) "b" @?= MatchSuccess

    it "has a failure message when inverted" $
      runMatch (no (hasItem 'a')) "a" @?= MatchFailure "Expected: Not 'a' to be in \"a\""

  describe "assertThat" $ do
    it "is used as an hunit test" $
      assertThat "a" (hasItem 'a')
