module Test.Rematch.QuickCheck where
import Test.QuickCheck.Property
import Test.QuickCheck.Monadic
import Control.Rematch
import Control.Rematch.Run

-- |Run a matcher as an QuickCheck assertion
--
-- Example output:
--
-- @
--Expected: equalTo "a"
--     but: was "b"
-- @
expectP :: a -> Matcher a -> Property
expectP a matcher = property $ (runMatch matcher a)

-- |Run a matcher as an QuickCheck assertion
--
-- Example output:
--
-- @
--Expected: equalTo "a"
--     but: was "b"
-- @
expectPM :: Monad m => a -> Matcher a -> PropertyM m ()
expectPM a matcher = case (runMatch matcher a) of
  MatchSuccess -> return ()
  (MatchFailure description) -> fail description

instance Testable Match where
  property = property . liftMatchResult
  exhaustive = const True

liftMatchResult :: Match -> Result
liftMatchResult MatchSuccess = succeeded
liftMatchResult (MatchFailure mismatch) = failed { reason = mismatch }
