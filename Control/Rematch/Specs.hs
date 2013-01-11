module Control.Rematch.Specs where
import Test.Hspec
import Test.HUnit
import Test.Hspec.HUnit()
import Control.Rematch

main :: IO ()
main = hspec $ do
  describe "rematch core" $ do
    it "can be inverted" $
      checkMatch (no (is 'a')) 'b' @?= Nothing

    it "has a failure message when inverted" $
      checkMatch (no (is 'a')) 'a' @?= Just ("not equalTo 'a'", "was 'a'")

  describe "assertThat" $ do
    it "is used as an hunit test" $
      assertThat "a" (hasItem 'a')

  generalPurposeMatcherSpecs
  combinedMatcherSpecs
  listMatcherSpecs
  joinSpecs

generalPurposeMatcherSpecs :: Spec
generalPurposeMatcherSpecs = describe "general purpose matchers" $ do
  describe "is" $ do
    it "matches on equality" $
      checkMatch (is 'a') 'a' @?= Nothing
    it "fails on inequality" $
      checkMatch (is 'b') 'a' @?= Just ("equalTo 'b'", "was 'a'")

combinedMatcherSpecs :: Spec
combinedMatcherSpecs = describe "combined matchers" $ do
  describe "allOf" $ do
    it "matches when all of the individual matchers match" $
      checkMatch (allOf [is 'a', is 'a']) 'a' @?= Nothing

    it "fails when you don't supply a matcher at all" $
      checkMatch (allOf []) 'a' @?= Just ("allOf", "was: no matchers supplied")

    it "fails if an individual matcher matches" $
      checkMatch (allOf [is 'a', is 'b']) 'a' @?= Just ("all(equalTo 'a', equalTo 'b')","(was 'a')")

  describe "anyOf" $ do
    it "matches when one of the individual matchers match" $
      checkMatch (anyOf [is 'a', is 'b']) 'a' @?= Nothing

    it "fails when you don't supply a matcher at all" $
      checkMatch (anyOf []) 'a' @?= Just ("anyOf", "was: no matchers supplied")

    it "fails if all the individual matchers fail" $
      checkMatch (anyOf [is 'a', is 'b']) 'c' @?= Just ("or(equalTo 'a', equalTo 'b')", "(was 'c', was 'c')")

listMatcherSpecs :: Spec
listMatcherSpecs = describe "list matchers" $ do
  describe "everyItem" $ do
    it "matches when every item matches" $
      checkMatch (everyItem (is 'a')) ['a'] @?= Nothing

    it "fails when one item fails the match" $
      checkMatch (everyItem (is 'b')) ['a'] @?= Just ("everyItem(equalTo 'b')", "(was 'a')")

    it "matches when there are no items" $
      checkMatch (everyItem (is 'b')) [] @?= Nothing

joinSpecs :: Spec
joinSpecs = describe "join" $ do
  it "doesn't do anything for an empty list" $
    join "a" [] @?= ""

  it "just displays a single elements list" $
    join ", " ["a"] @?= "a"

  it "joins a list of two items with the separator" $
    join ", " ["a", "b"] @?= "a, b"

checkMatch :: Matcher a -> a -> Maybe (String, String)
checkMatch m a = if (match m $ a)
  then Nothing
  else Just (description m, describeMismatch m $ a)
