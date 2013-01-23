module Control.Rematch.Specs where
import Test.Hspec
import Test.HUnit
import Test.Hspec.HUnit()
import Control.Rematch
import Control.Rematch.Formatting

main :: IO ()
main = hspec $ do
  describe "rematch core" $ do
    it "can be inverted" $
      checkMatch (no (is 'a')) 'b' @?= Nothing

    it "has a failure message when inverted" $
      checkMatch (no (is 'a')) 'a' @?= Just ("not equalTo 'a'", "was 'a'")

  describe "expect" $
    it "is used as an hunit test" $
      expect "a" (is "a")

  generalPurposeMatcherSpecs
  combinedMatcherSpecs
  listMatcherSpecs
  comparableSpecs
  joinSpecs

generalPurposeMatcherSpecs :: Spec
generalPurposeMatcherSpecs = describe "general purpose matchers" $
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
      checkMatch (everyItem (is 1)) [1] @?= Nothing

    it "fails when one item fails the match" $
      checkMatch (everyItem (is 1)) [3] @?= Just ("everyItem(equalTo 1)", "(was 3)")

    it "matches when there are no items" $
      checkMatch (everyItem (is 1)) [] @?= Nothing

    it "only shows match failures for failing items" $
      checkMatch (everyItem (is 1)) [1, 3] @?= Just ("everyItem(equalTo 1)", "(was 3)")

  describe "hasItem" $ do
    it "matches when one item matches" $
      checkMatch (hasItem (equalTo 1)) [1, 3] @?= Nothing

    it "fails when none of them match" $
      checkMatch (hasItem (equalTo 1)) [3] @?= Just ("hasItem(equalTo 1)", "(was 3)")

    it "fails when there are no items" $
      checkMatch (hasItem (equalTo 3)) [] @?= Just ("hasItem(equalTo 3)", "got an empty list: []")

  describe "isEmpty" $ do
    it "matches when the thing is empty" $
      checkMatch isEmpty ([] :: String) @?= Nothing

    it "fails when the thing has items" $
      checkMatch isEmpty [1] @?= Just ("isEmpty", "was [1]")

  it "hasSize" pending

comparableSpecs :: Spec
comparableSpecs = describe "comparables" $ do
  describe "greaterThan" $ do
    it "matches when the item is greater" $
      checkMatch (greaterThan 5) 6 @?= Nothing

    it "fails when the item is lesser" $
      checkMatch (greaterThan 5) 4 @?= Just ("greaterThan(5)", "was 4")

  describe "lessThan" $ do
    it "matches when the item is lesser" $
      checkMatch (lessThan 5) 4 @?= Nothing

    it "fails when the item is greater" $
      checkMatch (lessThan 5) 6 @?= Just ("lessThan(5)", "was 6")

joinSpecs :: Spec
joinSpecs = describe "join" $ do
  it "doesn't do anything for an empty list" $
    join "a" [] @?= ""

  it "just displays a single elements list" $
    join ", " ["a"] @?= "a"

  it "joins a list of two items with the separator" $
    join ", " ["a", "b"] @?= "a, b"

checkMatch :: Matcher a -> a -> Maybe (String, String)
checkMatch m a = if match m a
  then Nothing
  else Just (description m, describeMismatch m a)
