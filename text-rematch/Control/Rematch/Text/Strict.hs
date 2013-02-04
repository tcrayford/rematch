module Control.Rematch.Text.Strict where
import Data.Text
import Data.Char(isSpace)
import qualified Data.Text as T
import Control.Rematch(Matcher(..), standardMismatch)

startsWith :: Text -> Matcher Text
startsWith t = Matcher {
    match = isPrefixOf t
  , description = "startsWith " ++ show t
  , describeMismatch = standardMismatch
  }

endsWith :: Text -> Matcher Text
endsWith t = Matcher {
    match = isSuffixOf t
  , description = "endsWith " ++ show t
  , describeMismatch = standardMismatch
  }

containsText :: Text -> Matcher Text
containsText t = Matcher {
    match = isInfixOf t
  , description = "containsText " ++ show t
  , describeMismatch = standardMismatch
  }

equalToIgnoringCase :: Text -> Matcher Text
equalToIgnoringCase t = Matcher {
    match = (== toLower t) . toLower
  , description = "equalToIgnoringCase " ++ show t
  , describeMismatch = standardMismatch
  }

equalToIgnoringWhitespace :: Text -> Matcher Text
equalToIgnoringWhitespace t = Matcher {
    match = (== removeWhitespace t) . removeWhitespace
  , description = "equalToIgnoringWhitespace " ++ show t
  , describeMismatch = standardMismatch
  }
  where removeWhitespace = T.filter (not . isSpace)
