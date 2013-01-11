module Control.Rematch where
import Test.HUnit

data Match = MatchSuccess | MatchFailure String deriving (Eq, Show)

data Matcher a = Matcher {
    match :: a -> Bool
  , description :: String
  , describeMismatch :: a -> String
  }

assertThat :: (Show a) => a -> Matcher a -> Assertion
assertThat a matcher = (runMatch matcher) a @?= MatchSuccess

expect :: (Show a) => a -> Matcher a -> Assertion
expect = assertThat

no :: Matcher a -> Matcher a
no (Matcher m desc mismatch) = Matcher (not . m) ("not " ++ desc) (\a -> (mismatch a))

runMatch :: Matcher a -> a -> Match
runMatch m a = if (match m $ a)
  then MatchSuccess
  else MatchFailure $ "Expected:\n " ++ description m ++ "\n  but:  " ++ (describeMismatch m $ a)

is :: (Show a, Eq a) => a -> Matcher a
is a = Matcher (a == ) ("equalTo " ++ show a) standardMismatch

equalTo :: (Show a, Eq a) => a -> Matcher a
equalTo = is

allOf :: (Show a, Eq a) => [Matcher a] -> Matcher a
allOf [] = Matcher (const False) "allOf" (const "was: no matchers supplied")
allOf matchers = Matcher {
    match = (and . matchList matchers)
  , description = describeList "all" $ map description matchers
  , describeMismatch = (\a -> describeList "" (map ((flip describeMismatch) a) (filter (\m -> not $ match m $ a) matchers)))
  }

matchList :: [Matcher a] -> a -> [Bool]
matchList matchers a = map (\m -> match m $ a) matchers

anyOf :: (Show a) => [Matcher a] -> Matcher a
anyOf [] = Matcher (const False) "anyOf" (const "was: no matchers supplied")
anyOf matchers = Matcher {
    match = (or . matchList matchers)
  , description = describeList "or" $ map description matchers
  , describeMismatch = (\a -> describeList "" (map ((flip describeMismatch) a) matchers))
  }

everyItem :: Matcher a -> Matcher [a]
everyItem m = Matcher {
    match = (and . map (match m))
  , description = "everyItem(" ++ description m ++ ")"
  , describeMismatch = (\as -> describeList "" (map (describeMismatch m) (filter (not . match m) as)))
  }

hasItem :: (Show a) => Matcher a -> Matcher [a]
hasItem m = Matcher {
    match = (or . map (match m))
  , description = "hasItem(" ++ description m ++ ")"
  , describeMismatch = go
  }
  where go [] = "got an empty list: []"
        go as = describeList "" (map (describeMismatch m) as)

containsInAnyOrder :: (Show a) => [Matcher a] -> Matcher [a]
containsInAnyOrder ms = anyOf (map hasItem ms)

describeList :: String -> [String] -> String
describeList start xs = start ++ "(" ++ join ", " xs ++ ")"

join :: String -> [String] -> String
join _ [] = ""
join _ [a] = a
join sep (x:xs) = x ++ sep ++ join sep xs

standardMismatch :: (Show a) => a -> String
standardMismatch a = "was " ++ show a