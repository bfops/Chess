{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | By importing this module, you solemnly swear that you will not call
--   'unsafeRunOnGraphicsCard' on anything except actual graphics commands.
--
--   This preserves the sanctity of the GL type, and makes it so we don't ever
--   go full retard while parallelizing.
module Graphics.Rendering.OpenGL.Monad.Unsafe ( GL
                                              , runGraphics
                                              , unsafeRunOnGraphicsCard
                                              ) where

import Control.Applicative
import Control.Monad

-- | A wrapper for any actions performed on a graphics card. This allows us to
--   have a concurrent, yet easily thread-safe main game loop. Thank god for a
--   powerful type system.
newtype GL a = GL (IO a)
    deriving (Functor, Applicative, Monad)

-- | Runs a set of graphics actions in the IO monad.
runGraphics :: GL a -> IO a
runGraphics (GL x) = x
{-# INLINE runGraphics #-}

-- | Converts a graphics action in the IO monad into one in the GL monad.
--
--   Please, for the love of god, only use this on _actual_ graphics commands.
--   Anything else defeats the point entirely.
unsafeRunOnGraphicsCard :: IO a -> GL a
unsafeRunOnGraphicsCard = GL
{-# INLINE unsafeRunOnGraphicsCard #-}
