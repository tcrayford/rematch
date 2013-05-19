module Control.Rematch.ByteString.Specs where
import Test.Hspec
import Test.HUnit
import Test.Hspec.HUnit()
import Control.Rematch
import qualified Control.Rematch.ByteString.Strict as S

main :: IO ()
main = hspec $ describe "rematch-bytestring" $ do
  describe "Control.Rematch.ByteString.Strict" $ do
    it "" $
      pending "todo"
  describe "Control.Rematch.ByteString.Lazy" $ do
    it "" $
      pending "todo"
