module Control.Rematch where
import Test.HUnit

data Match = MatchSuccess | MatchFailure String deriving (Eq, Show)

data Matcher a = Matcher {
    match :: a -> Bool
  , describeMatch :: String
  }

hasItem :: (Eq a, Show a) => a -> Matcher [a]
hasItem a = Matcher (a `elem`) $ show a ++ " to be in"

assertThat :: (Show a) => a -> Matcher a -> Assertion
assertThat a matcher = (runMatch matcher) a @?= MatchSuccess

no :: Matcher a -> Matcher a
no (Matcher m description) = Matcher go ("Not " ++ description)
  where go a = not $ m a

runMatch :: (Show a) => Matcher a -> a -> Match
runMatch m a = if (match m $ a)
  then MatchSuccess
  else MatchFailure ("Expected: " ++ describeMatch m ++ " " ++ show a)

is :: (Show a, Eq a) => a -> Matcher a
is a = Matcher (a == ) $ show a ++ " to equal"
