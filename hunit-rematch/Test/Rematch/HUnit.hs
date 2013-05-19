module Test.Rematch.HUnit where
import Test.HUnit(Assertion, assertFailure)
import Control.Rematch
import Control.Rematch.Run

-- |Run a matcher as an HUnit assertion
--
-- Example output:
--
-- @
--Expected: equalTo "a"
--     but: was "b"
-- @
expect :: a -> Matcher a -> Assertion
expect a matcher = case res of
  MatchSuccess -> return ()
  (MatchFailure msg) -> assertFailure msg
  where res = runMatch matcher a
