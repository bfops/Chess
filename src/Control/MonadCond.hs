module Control.MonadCond where

import Control.Monad

-- | Conditionally create a monad
mcond :: MonadPlus m
      => a
      -- ^ Value to make into a monad.
      -> Bool
      -- ^ If this condition is false, mzero.
      -- Otherwise, return the value.
      -> m a
mcond x b = guard b >> return x

-- | Conditionally nullify a monad
ifm :: MonadPlus m
      => Bool
      -- ^ If this condition is false, mzero.
      -- Otherwise, return the monad.
      -> m a
      -- ^ Monad to filter
      -> m a
ifm = (>>) . guard
