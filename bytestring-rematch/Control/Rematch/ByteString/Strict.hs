module Control.Rematch.ByteString.Strict where
import Control.Rematch(Matcher(..), standardMismatch)
import qualified Data.ByteString.Char8 as B

startsWith :: B.ByteString -> Matcher B.ByteString
startsWith b = Matcher {
    match = B.isPrefixOf b
  , description = "startsWith " ++ show b
  , describeMismatch = standardMismatch
  }

endsWith :: B.ByteString -> Matcher B.ByteString
endsWith b = Matcher {
    match = B.isSuffixOf b
  , description = "endsWith " ++ show b
  , describeMismatch = standardMismatch
  }

containsText :: B.ByteString -> Matcher B.ByteString
containsText b = Matcher {
    match = B.isInfixOf b
  , description = "containsText " ++ show b
  , describeMismatch = standardMismatch
  }

isEmptyByteString :: Matcher B.ByteString
isEmptyByteString = Matcher {
    match = B.null
  , description = "isEmptyByteString"
  , describeMismatch = standardMismatch
  }
