import qualified Control.Rematch.ByteString.Specs as BS
import qualified Test.Rematch.Specs.HUnit as HS
import qualified Test.Rematch.QuickCheckSpecs as QC
import qualified Control.Rematch.Text.Specs as TS
import qualified Control.Rematch.Specs as RS
import Test.Hspec

main :: IO ()
main = hspec $ do
  BS.specs
  HS.specs
  QC.specs
  TS.specs
  RS.specs
