module Control.Rematch.Run where

-- | A type representing a match success or failure.
data Match = MatchSuccess | MatchFailure String deriving (Eq, Show)
