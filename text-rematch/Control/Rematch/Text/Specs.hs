{-# LANGUAGE OverloadedStrings #-}
module Control.Rematch.Text.Specs where
import Test.Hspec
import Test.HUnit
import Test.Hspec.HUnit()
import Control.Rematch
import Control.Rematch.Text.Strict

main :: IO ()
main = hspec $ specs

specs :: Spec
specs = describe "rematch-text" $ do
  describe "startsWith" $ do
    it "passes when the input begins with the query" $
      checkMatch "abcd" (startsWith "ab") @?= Nothing

    it "fails when the input does not being with the query" $
      checkMatch "zyx" (startsWith "ab") @?= Just ("startsWith \"ab\"", "was \"zyx\"")

  describe "endsWith" $ do
    it "matches when the input ends with the query" $
      checkMatch "abcd" (endsWith "cd") @?= Nothing

    it "matches when the input does not end with the query" $
      checkMatch "abc" (endsWith "cd") @?= Just ("endsWith \"cd\"", "was \"abc\"")

  describe "containsText" $ do
    it "matches when the input contains the text" $
      checkMatch "abcd" (containsText "bc") @?= Nothing

    it "fails when the input does not contain the text" $
      checkMatch "abd" (containsText "bc") @?= Just ("containsText \"bc\"", "was \"abd\"")

  describe "equalToIgnoringCase" $ do
    it "matches when the input is equal with different case" $
      checkMatch "Abcd" (equalToIgnoringCase "aBcD") @?= Nothing

    it "fails when the input isn't equal after case conversion" $
      checkMatch "abd" (equalToIgnoringCase "bc") @?= Just ("equalToIgnoringCase \"bc\"", "was \"abd\"")

  describe "equalToIgnoringWhitespace" $ do
    it "matches when the input is equal with different whitespace" $
      checkMatch "   abcd" (equalToIgnoringWhitespace "abcd    ") @?= Nothing

    it "fails when the input is different" $
      checkMatch "abd" (equalToIgnoringWhitespace "bc") @?= Just ("equalToIgnoringWhitespace \"bc\"", "was \"abd\"")

  describe "isEmptyText" $ do
    it "matches when the input is empty" $
      checkMatch "" isEmptyText @?= Nothing

    it "fails when the input has content" $
      checkMatch "asdf" isEmptyText @?= Just ("isEmptyText", "was \"asdf\"")

checkMatch :: a -> Matcher a -> Maybe (String, String)
checkMatch a m = if match m a
  then Nothing
  else Just (description m, describeMismatch m a)
