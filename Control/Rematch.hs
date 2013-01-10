module Control.Rematch where
import Test.HUnit

data Match = MatchSuccess | MatchFailure String deriving (Eq, Show)

data Matcher a = Matcher {
    match :: a -> Bool
  , description :: String
  , describeMismatch :: a -> String
  }

-- XXX: rework
hasItem :: (Eq a, Show a) => a -> Matcher [a]
hasItem a = Matcher (a `elem`) "has" (\b  -> "was " ++ show b)

assertThat :: (Show a) => a -> Matcher a -> Assertion
assertThat a matcher = (runMatch matcher) a @?= MatchSuccess

no :: Matcher a -> Matcher a
no (Matcher m desc mismatch) = Matcher (not . m) ("not " ++ desc) (\a -> (mismatch a))

runMatch :: Matcher a -> a -> Match
runMatch m a = if (match m $ a)
  then MatchSuccess
  else MatchFailure $ "Expected:\n " ++ description m ++ "\n  but:  " ++ (describeMismatch m $ a)

is :: (Show a, Eq a) => a -> Matcher a
is a = Matcher (a == ) ("equalTo " ++ show a) (\b -> "was " ++ show b)

allOf :: (Show a, Eq a) => [Matcher a] -> Matcher a
allOf [] = Matcher (const False) "allOf" (const "was: no matchers supplied")
allOf matchers = Matcher {
    match = (\a -> and $ map (\m -> match m a) matchers)
  , description = describeList "all" $ map description matchers
  , describeMismatch = (\a -> describeList "was" (map ((flip describeMismatch) a) (filter (\m -> not $ match m $ a) matchers)))
  }

describeList :: String -> [String] -> String
describeList start xs = start ++ "(" ++ join ", " xs ++ ")"

join :: String -> [String] -> String
join _ [] = ""
join _ [a] = a
join sep (x:xs) = x ++ sep ++ join sep xs
