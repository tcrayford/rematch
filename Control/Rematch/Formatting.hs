-- |This module contains some utility functions for formatting descriptions
-- It is probably only useful when you're writing your own matchers
module Control.Rematch.Formatting where

-- |Utility function for formatting a list of strings like the following.
-- For example, describeList "anyOf" ["is 'a'"] == "anyOf(is 'a')"
describeList :: String -> [String] -> String
describeList start xs = start ++ "(" ++ join ", " xs ++ ")"

-- |Utility function for formatting a list of strings with a separator
join :: String -> [String] -> String
join _ [] = ""
join _ [a] = a
join sep (x:xs) = x ++ sep ++ join sep xs
