import qualified Control.Rematch.HashMap.Specs as M
import qualified Control.Rematch.HashSet.Specs as H
import Test.Hspec

main :: IO ()
main = hspec $ specs

specs :: Spec
specs = do
  M.specs
  H.specs
