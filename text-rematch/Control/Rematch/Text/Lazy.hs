-- |This module exports `rematch` matchers for strict Data.Text
module Control.Rematch.Text.Lazy where
import Data.Text.Lazy
import Data.Char(isSpace)
import qualified Data.Text.Lazy as T
import Control.Rematch(Matcher(..), standardMismatch)

-- |matchers if the input begins with some Text
-- becomes success
startsWith :: Text -> Matcher Text
startsWith t = Matcher {
    match = isPrefixOf t
  , description = "startsWith " ++ show t
  , describeMismatch = standardMismatch
  }

-- |matchers if the input ends with some Text
endsWith :: Text -> Matcher Text
endsWith t = Matcher {
    match = isSuffixOf t
  , description = "endsWith " ++ show t
  , describeMismatch = standardMismatch
  }

-- |matchers if the input contains some Text
containsText :: Text -> Matcher Text
containsText t = Matcher {
    match = isInfixOf t
  , description = "containsText " ++ show t
  , describeMismatch = standardMismatch
  }

-- |matchers if the input is equal ignoring case
equalToIgnoringCase :: Text -> Matcher Text
equalToIgnoringCase t = Matcher {
    match = (== toLower t) . toLower
  , description = "equalToIgnoringCase " ++ show t
  , describeMismatch = standardMismatch
  }

-- |matchers if the input is equal ignoring whitespace
equalToIgnoringWhitespace :: Text -> Matcher Text
equalToIgnoringWhitespace t = Matcher {
    match = (== removeWhitespace t) . removeWhitespace
  , description = "equalToIgnoringWhitespace " ++ show t
  , describeMismatch = standardMismatch
  }
  where removeWhitespace = T.filter (not . isSpace)

-- |matchers if the input is empty Text
isEmptyText :: Matcher Text
isEmptyText = Matcher {
    match = T.null
  , description = "isEmptyText"
  , describeMismatch = standardMismatch
  }
