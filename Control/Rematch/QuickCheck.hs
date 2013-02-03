module Control.Rematch.QuickCheck where
import Test.QuickCheck.Property
import Control.Rematch
import Control.Rematch.Run

expectP :: a -> Matcher a -> Property
expectP a matcher = property $ (runMatch matcher a)

instance Testable Match where
  property = property . liftMatchResult
  exhaustive = const True

liftMatchResult :: Match -> Result
liftMatchResult MatchSuccess = succeeded
liftMatchResult (MatchFailure mismatch) = failed { reason = mismatch }
