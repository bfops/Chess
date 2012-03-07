-- | Contains helpers for dealing with cyclic enums. Aka, enums which, when
--   incremented enough, wrap around.
--
--   Make sure to import this module qualified, since 'next' and 'prev' are
--   pretty common words.
module Data.Cycle ( next
                  , prev
                  ) where

-- | Increment a, possibly wrapping around.
next :: (Enum a, Bounded a, Eq a) => a -> a
next a = if a == maxBound
         then minBound
         else succ a

-- | Decrement a, possibly wrapping around.
prev :: (Enum a, Bounded a, Eq a) => a -> a
prev a = if a == minBound
         then maxBound
         else pred a

