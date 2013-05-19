module Control.Rematch.HashMap.Lazy where
import Data.Hashable
import Data.HashMap.Lazy
import Data.HashMap.Lazy as M
import Control.Rematch

isEmptyMap :: (Show k, Show v, Hashable k) => Matcher (HashMap k v)
isEmptyMap = Matcher {
    match = M.null
  , description = "isEmptyMap"
  , describeMismatch = standardMismatch
  }

hasSize :: (Show k, Show v, Hashable k) => Int -> Matcher (HashMap k v)
hasSize n = Matcher {
    match = (== n) . M.size
  , description = "hasSize " ++ show n
  , describeMismatch = standardMismatch
  }

hasKey :: (Show k, Show v, Hashable k, Eq k) => k -> Matcher (HashMap k v)
hasKey k = Matcher {
    match = M.member k
  , description = "hasKey " ++ show k
  , describeMismatch = standardMismatch
  }

hasValueAt :: (Show k, Show v, Hashable k, Eq k, Eq v) => k -> v -> Matcher (HashMap k v)
hasValueAt k v = Matcher {
    match = (== Just v) . (M.lookup k)
  , description = "hasValueAt " ++ show k ++ " " ++ show v
  , describeMismatch = (\m -> "was " ++ show (M.lookup k m))
 }

containsMap :: (Show k, Show v, Hashable k, Eq k) => HashMap k v -> Matcher (HashMap k v)
containsMap m = Matcher {
    match = (M.null) . (M.difference m)
  , description = "containsMap " ++ show m
  , describeMismatch = standardMismatch
  }

hasKeys :: (Show k, Show v, Hashable k, Eq k) => [k] -> Matcher (HashMap k v)
hasKeys ks = Matcher {
    match = (== ks) . M.keys
  , description = "hasKeys " ++ show ks
  , describeMismatch = (\m -> "had keys " ++ show (M.keys m))
  }

hasValues :: (Show k, Show v, Hashable k, Eq v) => [v] -> Matcher (HashMap k v)
hasValues vs = Matcher {
    match = (== vs) . M.elems
  , description = "hasValues " ++ show vs
  , describeMismatch = (\m -> "had values " ++ show (M.elems m))
  }
