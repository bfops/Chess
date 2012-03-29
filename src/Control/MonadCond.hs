module Control.MonadCond where

import Control.Monad

-- ? Conditionally create a monad
(<?>) :: MonadPlus m
      => a
      -- ^ Value to make into a monad.
      -> Bool
      -- ^ If this condition is false, mzero.
      -- Otherwise, return the value.
      -> m a
(<?>) = (>>?) . return

-- ? Conditionally nullify a monad
(>>?) :: MonadPlus m
      => m a
      -- ^ Monad to filter
      -> Bool
      -- ^ If this condition is false, mzero.
      -- Otherwise, return the monad.
      -> m a
(>>?) = flip (?<<)

-- ? (>>?) with arguments reversed.
(?<<) :: MonadPlus m => Bool -> m a -> m a
(?<<) = (>>) . guard
