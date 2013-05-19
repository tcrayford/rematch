module Control.Rematch.HashSet where
import Data.HashSet
import Data.Hashable
import qualified Data.HashSet as H
import Control.Rematch(Matcher(..), standardMismatch)

isEmpty :: (Show a) => Matcher (HashSet a)
isEmpty = Matcher {
    match = H.null
  , description = "isEmpty"
  , describeMismatch = standardMismatch
  }

hasSize :: (Show a) => Int -> Matcher (HashSet a)
hasSize n = Matcher {
    match = (== n) . H.size
  , description = "hasSize " ++ show n
  , describeMismatch = (\m -> "had size " ++ show (H.size m))
  }

hasMember :: (Show a, Eq a, Hashable a) => a -> Matcher (HashSet a)
hasMember x = Matcher {
    match = H.member x
  , description = "hasMember " ++ show x
  , describeMismatch = standardMismatch
  }

containsSet :: (Show a, Eq a, Hashable a) => HashSet a -> Matcher (HashSet a)
containsSet h = Matcher {
    match = H.null . H.difference h
  , description = "containsSet " ++ show h
  , describeMismatch = standardMismatch
  }
