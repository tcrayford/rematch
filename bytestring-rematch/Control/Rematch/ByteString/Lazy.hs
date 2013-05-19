module Control.Rematch.ByteString.Lazy where
import Control.Rematch(Matcher(..))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as S

startsWith :: B.ByteString -> Matcher B.ByteString
startsWith b = Matcher {
    match = B.isPrefixOf b
  , description = "startsWith " ++ show (toStrict b)
  , describeMismatch = standardMismatch
  }

endsWith :: B.ByteString -> Matcher B.ByteString
endsWith b = Matcher {
    match = (S.isSuffixOf (toStrict b)) . toStrict
  , description = "endsWith " ++ show (toStrict b)
  , describeMismatch = standardMismatch
  }

containsText :: B.ByteString -> Matcher B.ByteString
containsText b = Matcher {
    match = (S.isInfixOf (toStrict b)) . toStrict
  , description = "containsText " ++ show (toStrict b)
  , describeMismatch = standardMismatch
  }

isEmptyByteString :: Matcher B.ByteString
isEmptyByteString = Matcher {
    match = B.null
  , description = "isEmptyByteString"
  , describeMismatch = standardMismatch
  }

standardMismatch :: B.ByteString -> String
standardMismatch b = "was " ++ show (toStrict b)

toStrict :: B.ByteString -> S.ByteString
toStrict = S.concat . B.toChunks
