{-# LANGUAGE OverloadedStrings #-}
module Control.Rematch.ByteString.Specs where
import Test.Hspec
import Test.HUnit
import Test.Hspec.HUnit()
import Control.Rematch
import qualified Control.Rematch.ByteString.Strict as S
import qualified Control.Rematch.ByteString.Lazy as L
import Data.ByteString.Char8

main :: IO ()
main = hspec $ describe "rematch-bytestring" $ do
  describe "Control.Rematch.ByteString.Strict" $ do
    describe "startsWith" $ do
      it "passes when the input begins with the query" $
        checkMatch "abcd" (S.startsWith "ab") @?= Nothing

      it "fails when the input does not being with the query" $
        checkMatch "zyx" (S.startsWith "ab") @?= Just ("startsWith \"ab\"", "was \"zyx\"")

    describe "endsWith" $ do
      it "matches when the input ends with the query" $
        checkMatch "abcd" (S.endsWith "cd") @?= Nothing

      it "matches when the input does not end with the query" $
        checkMatch "abc" (S.endsWith "cd") @?= Just ("endsWith \"cd\"", "was \"abc\"")

    describe "containsText" $ do
      it "matches when the input contains the text" $
        checkMatch "abcd" (S.containsText "bc") @?= Nothing

      it "fails when the input does not contain the text" $
        checkMatch "abd" (S.containsText "bc") @?= Just ("containsText \"bc\"", "was \"abd\"")

    describe "isEmptyByteString" $ do
      it "matches when the input is empty" $
        checkMatch "" S.isEmptyByteString @?= Nothing

      it "fails when the input has content" $
        checkMatch "asdf" S.isEmptyByteString @?= Just ("isEmptyByteString", "was \"asdf\"")

  describe "Control.Rematch.ByteString.Lazy" $ do
    describe "startsWith" $ do
      it "passes when the input begins with the query" $
        checkMatch "abcd" (L.startsWith "ab") @?= Nothing

      it "fails when the input does not being with the query" $
        checkMatch "zyx" (L.startsWith "ab") @?= Just ("startsWith \"ab\"", "was \"zyx\"")

    describe "endsWith" $ do
      it "matches when the input ends with the query" $
        checkMatch "abcd" (L.endsWith "cd") @?= Nothing

      it "matches when the input does not end with the query" $
        checkMatch "abc" (L.endsWith "cd") @?= Just ("endsWith \"cd\"", "was \"abc\"")

    describe "containsText" $ do
      it "matches when the input contains the text" $
        checkMatch "abcd" (L.containsText "bc") @?= Nothing

      it "fails when the input does not contain the text" $
        checkMatch "abd" (L.containsText "bc") @?= Just ("containsText \"bc\"", "was \"abd\"")

    describe "isEmptyByteString" $ do
      it "matches when the input is empty" $
        checkMatch "" L.isEmptyByteString @?= Nothing

      it "fails when the input has content" $
        checkMatch "asdf" L.isEmptyByteString @?= Just ("isEmptyByteString", "was \"asdf\"")

checkMatch :: a -> Matcher a -> Maybe (String, String)
checkMatch a m = if match m a
  then Nothing
  else Just (description m, describeMismatch m a)
