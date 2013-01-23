-- |This module defines an api for matchers: rules that can pass or fail,
-- and describe their failure and success conditions for humans to read.
--
-- This module also exports some useful matchers for things in the "Prelude",
-- and some combinators that are useful for combining several matchers into one.
module Control.Rematch(
    Matcher(..)
  -- ** Useful functions for running matchers
  , expect
  , runMatch
  -- ** Basic Matchers
  , is
  , equalTo
  -- ** Matchers on lists
  , isEmpty
  , hasSize
  , everyItem
  , hasItem
  -- ** Matchers on Ord
  , greaterThan
  , greaterThanOrEqual
  , lessThan
  , lessThanOrEqual
  -- ** Matchers on Maybe
  , isJust
  , isNothing
  -- ** Matchers on Either
  , isRight
  , isLeft
  -- ** Matcher combinators
  , isNot
  , allOf
  , anyOf
  -- ** Utility functions for writing your own matchers
  , matcherOn
  , matchList
  , standardMismatch
  ) where
import Test.HUnit(Assertion, assertFailure)
import qualified Data.Maybe as M
import Control.Rematch.Run
import Control.Rematch.Formatting

-- |The basic api for a matcher
data Matcher a = Matcher {
    match :: a -> Bool
  -- ^ A function that returns True if the matcher should pass, False if it should fail
  , description :: String
  -- ^ A description of the matcher (usually of its success conditions)
  , describeMismatch :: a -> String
  -- ^ A description to be shown if the match fails.
  }

-- |Run a matcher as an HUnit assertion
--
-- Example output:
--
-- @
-- Expected:
-- equalTo \"a\"
-- but:  was \"b\"
-- @
expect :: a -> Matcher a -> Assertion
expect a matcher = case res of
  MatchSuccess -> return ()
  (MatchFailure msg) -> assertFailure msg
  where res = runMatch matcher a

-- |Inverts a matcher, so success becomes failure, and failure
-- becomes success
isNot :: Matcher a -> Matcher a
isNot (Matcher m desc mismatch) = Matcher (not . m) ("isNot " ++ desc) mismatch

-- |Run a matcher, producing a Match with a good error string
runMatch :: Matcher a -> a -> Match
runMatch m a = if match m a
  then MatchSuccess
  else MatchFailure $ "Expected:\n " ++ description m ++ "\n  but:  " ++ describeMismatch m a

-- |Matcher on equality
is :: (Show a, Eq a) => a -> Matcher a
is a = Matcher (a == ) ("equalTo " ++ show a) standardMismatch

-- |Matcher on equality
equalTo :: (Show a, Eq a) => a -> Matcher a
equalTo = is

-- |Matches if all of a list of matchers pass
allOf :: [Matcher a] -> Matcher a
allOf [] = Matcher (const False) "allOf" (const "was: no matchers supplied")
allOf matchers = Matcher {
    match = and . matchList matchers
  , description = describeList "all" $ map description matchers
  , describeMismatch = \a -> describeList "" (map (`describeMismatch` a) (filter (\m -> not $ match m a) matchers))
  }

-- |Matches if any of a list of matchers pass
anyOf :: [Matcher a] -> Matcher a
anyOf [] = Matcher (const False) "anyOf" (const "was: no matchers supplied")
anyOf matchers = Matcher {
    match = or . matchList matchers
  , description = describeList "or" $ map description matchers
  , describeMismatch = \a -> describeList "" (map (`describeMismatch` a) matchers)
  }

-- |Matches if every item in the input list passes a matcher
everyItem :: Matcher a -> Matcher [a]
everyItem m = Matcher {
    match = all (match m)
  , description = "everyItem(" ++ description m ++ ")"
  , describeMismatch = describeList "" . map (describeMismatch m) . filter (not . match m)
  }

-- |Matches if any of the items in the input list passes the provided matcher
hasItem :: Matcher a -> Matcher [a]
hasItem m = Matcher {
    match = any (match m)
  , description = "hasItem(" ++ description m ++ ")"
  , describeMismatch = go
  }
  where go [] = "got an empty list: []"
        go as = describeList "" (map (describeMismatch m) as)

-- |Matches if the input list is empty
isEmpty :: (Show a) => Matcher [a]
isEmpty = Matcher {
    match = null
  , description = "isEmpty"
  , describeMismatch = standardMismatch
  }

-- |Matches if the input list has the required size
hasSize :: (Show a) => Int -> Matcher [a]
hasSize n = Matcher {
    match = ((== n) . length)
  , description = "hasSize(" ++ show n ++ ")"
  , describeMismatch = standardMismatch
  }

-- |Builds a Matcher a out of a name and a function from (a -> a -> Bool)
-- Succeeds if the function returns true, fails if the function returns false
matcherOn :: (Show a) => String -> (a -> a -> Bool) -> a -> Matcher a
matcherOn name comp a = Matcher {
    match = comp a
  , description = name ++ "(" ++ show a ++ ")"
  , describeMismatch = standardMismatch
  }

-- |Matches if the input is greater than the required number
greaterThan :: (Ord a, Show a) => a -> Matcher a
greaterThan = matcherOn "greaterThan" (<)

-- |Matches if the input is greater than or equal to the required number
greaterThanOrEqual :: (Ord a, Show a) => a -> Matcher a
greaterThanOrEqual = matcherOn "greaterThanOrEqual" (<=)

-- |Matches if the input is less than the required number
lessThan :: (Ord a, Show a) => a -> Matcher a
lessThan = matcherOn "lessThan" (>)

-- |Matches if the input is less than or equal to the required number
lessThanOrEqual :: (Ord a, Show a) => a -> Matcher a
lessThanOrEqual = matcherOn "lessThanOrEqual" (>=)

-- |Matches if the input is (Just a)
isJust :: (Show a) => Matcher (Maybe a)
isJust = Matcher {
    match = M.isJust
  , description = "isJust"
  , describeMismatch = standardMismatch
  }

-- |Matches if the input is Nothing
isNothing :: (Show a) => Matcher (Maybe a)
isNothing = Matcher {
    match = M.isNothing
  , description = "isNothing"
  , describeMismatch = standardMismatch
  }

-- |Matches if an Either is Right
isRight :: (Show a, Show b) => Matcher (Either a b)
isRight = Matcher {
    match = go
  , description = "isRight"
  , describeMismatch = standardMismatch
  }
  where go (Right _) = True
        go (Left _) = False

-- |Matches if an Either is Left
isLeft :: (Show a, Show b) => Matcher (Either a b)
isLeft = Matcher {
    match = go
  , description = "isLeft"
  , describeMismatch = standardMismatch
  }
  where go (Left _) = True
        go (Right _) = False

-- |Utility function for running a list of matchers
matchList :: [Matcher a] -> a -> [Bool]
matchList matchers a = map (`match` a) matchers

-- |A standard mismatch description on (Show a):
-- standardMismatch 1 == "was 1"
standardMismatch :: (Show a) => a -> String
standardMismatch a = "was " ++ show a
