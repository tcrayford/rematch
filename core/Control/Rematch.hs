-- |This module defines an api for matchers: rules that can pass or fail,
-- and describe their failure and success conditions for humans to read.
--
-- This module also exports some useful matchers for things in the "Prelude",
-- and some combinators that are useful for combining several matchers into one.
module Control.Rematch(
    Matcher(..)
  -- ** Useful functions for running matchers
  , runMatch
  -- ** Basic Matchers
  , is
  , equalTo
  -- ** Matchers on lists
  , isEmpty
  , isSingleton
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
  , hasJust
  , isNothing
  -- ** Matchers on Either
  , isRight
  , hasRight
  , isLeft
  , hasLeft
  -- ** Matcher combinators
  , isNot
  , allOf
  , anyOf
  , on
  , andAlso
  , followedBy
  -- ** Utility functions for writing your own matchers
  , matcherOn
  , matchList
  , standardMismatch
  ) where
import Control.Applicative (liftA2)
import Data.List ( nub
                 , intercalate )
import qualified Data.Maybe as M
import qualified Data.Foldable as F
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

-- |Inverts a matcher, so success becomes failure, and failure
-- becomes success
isNot :: Matcher a -> Matcher a
isNot (Matcher m desc mismatch) = Matcher (not . m) ("isNot " ++ desc) mismatch

-- |Run a matcher, producing a Match with a good error string
runMatch :: Matcher a -> a -> Match
runMatch m a = if match m a
  then MatchSuccess
  else MatchFailure $ "\nExpected: " ++ description m ++ "\n     but: " ++ describeMismatch m a

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

-- |A combinator that translates Matcher a to Matcher b using
-- a function :: (a -> b)
-- Takes a name of the function for better error messages
--
-- Using this as an infix operator gets you some nice syntax:
-- expect ((is 1) `on` (length, "length")) []
on :: Matcher b -> ((a -> b), String) -> Matcher a
on m (f, name) = Matcher {
    match = match m . f
  , description = name ++ " " ++ (description m)
  , describeMismatch = describeMismatch m  . f
  }

-- |A combinator that can be used as infix between to matchers on the same
-- type. For example:
-- betweenFiveAndTen :: Matcher Int
-- betweenFiveAndTen = greaterThan 5 `andAlso` lessThan 10
andAlso :: Matcher a -> Matcher a -> Matcher a
andAlso m m' = Matcher {
    match = liftA2 (&&) match1 match2
  , description = description m ++ " and " ++ description m'
  , describeMismatch = combineMismatch
  }
  where describeMismatch1 = describeMismatch m
        describeMismatch2 = describeMismatch m'
        match1            = match m
        match2            = match m'
        collapseMismatch  = intercalate " and " . nub
        combineMismatch x
          | not (match1 x) && not (match2 x) = collapseMismatch [describeMismatch1 x, describeMismatch2 x]
          | match1 x                         = describeMismatch1 x
          | match2 x                         = describeMismatch2 x
          | otherwise                        = "You've found a bug in rematch!"

-- |Matches if the matcher in the first argument matches the first item in the
-- input and the second argument matches the remaining items.  Designed to be
-- used infix, e.g. 'is 5 `followedBy` is 6 `followedBy` isEmpty' can be used to
-- match the list [5,6].
followedBy :: (Show a, Foldable f) => Matcher a -> Matcher [a] -> Matcher (f a)
followedBy l r = Matcher {
    match = doMatch . F.toList
  , description = description l ++ " followed by " ++ description r
  , describeMismatch = doDescribe . F.toList
  }
  where doMatch [] = False
        doMatch (x:xs) = match l x && match r xs
        doDescribe [] = "got an empty list: []"
        doDescribe (x:[]) | match l x = "matched " ++ show x
                          | otherwise = standardMismatch x
        doDescribe (x:xs) | match l x = "matched " ++ show x ++ ", " ++ describeMismatch r xs
                          | otherwise = standardMismatch x ++ ", " ++ describeMismatch r xs
infixr 0 `followedBy`
    
-- |Matches a Foldable instance if it contains exactly one item that passes a
-- matcher
isSingleton :: (Show a, Foldable f) => Matcher a -> Matcher (f a)
isSingleton m = Matcher {
    match = \x -> (length x == 1) && all (match m) x
  , description = "isSingleton(" ++ description m ++ ")"
  , describeMismatch = go . F.toList
  }
  where go [] = "got an empty list: []"
        go (x:[]) = describeMismatch m x
        go (x:xs) = "got a list with multiple items: " ++ show (x:xs)

-- |Matches if every item in the input list passes a matcher
everyItem :: Foldable f => Matcher a -> Matcher (f a)
everyItem m = Matcher {
    match = all (match m)
  , description = "everyItem(" ++ description m ++ ")"
  , describeMismatch = describeList "" . fmap (describeMismatch m) .
                       filter (not . match m) . F.toList
  }

-- |Matches if any of the items in the input list passes the provided matcher
hasItem :: Foldable f => Matcher a -> Matcher (f a)
hasItem m = Matcher {
    match = any (match m)
  , description = "hasItem(" ++ description m ++ ")"
  , describeMismatch = go . F.toList
  }
  where go [] = "got an empty list: []"
        go as = describeList "" (map (describeMismatch m) as)

-- |Matches if the input list is empty
isEmpty :: (Show (f a), Foldable f) => Matcher (f a)
isEmpty = Matcher {
    match = null
  , description = "isEmpty"
  , describeMismatch = standardMismatch
  }

-- |Matches if the input list has the required size
hasSize :: (Show (f a), Foldable f) => Int -> Matcher (f a)
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

-- |Matcher combinator, turns Matcher a to Matcher (Maybe a)
-- Fails if the Maybe is Nothing, otherwise tries the original
-- matcher on the content of the Maybe
hasJust :: Matcher a -> Matcher (Maybe a)
hasJust matcher = Matcher {
    match = (\a -> M.isJust a && (match matcher (M.fromJust a)))
  , description = "hasJust(" ++ description matcher ++ ")"
  , describeMismatch = mismatchDescription
  }
  where mismatchDescription (Just x) = matcher `describeMismatch` x
        mismatchDescription Nothing  = "but was Nothing"

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

-- |Matcher combinator: turns a Matcher b into a Matcher on the
-- Right side of an Either a b
hasRight :: (Show a, Show b) => Matcher b -> Matcher (Either a b)
hasRight matcher = Matcher {
    match = (\e -> case e of
                (Right a) -> match matcher a
                (Left _) -> False)
  , description = "hasRight(" ++ description matcher ++ ")"
  , describeMismatch = standardMismatch
  }

-- |Matches if an Either is Left
isLeft :: (Show a, Show b) => Matcher (Either a b)
isLeft = Matcher {
    match = go
  , description = "isLeft"
  , describeMismatch = standardMismatch
  }
  where go (Left _) = True
        go (Right _) = False

-- |Matcher combinator: turns a Matcher a into a Matcher on the
-- Left side of an Either a b
hasLeft :: (Show a, Show b) => Matcher a -> Matcher (Either a b)
hasLeft matcher = Matcher {
    match = (\e -> case e of
                (Left a) -> match matcher a
                (Right _) -> False)
  , description = "hasRight(" ++ description matcher ++ ")"
  , describeMismatch = standardMismatch
  }


-- |Utility function for running a list of matchers
matchList :: [Matcher a] -> a -> [Bool]
matchList matchers a = map (`match` a) matchers

-- |A standard mismatch description on (Show a):
-- standardMismatch 1 == "was 1"
standardMismatch :: (Show a) => a -> String
standardMismatch a = "was " ++ show a
