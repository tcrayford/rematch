module Control.Rematch.Text.Strict where
import Data.Text
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
