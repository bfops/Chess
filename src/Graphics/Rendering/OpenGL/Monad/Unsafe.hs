{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | By importing this module, you solemnly swear that you will not call
--   'unsafeRunOnGraphicsCard' on anything except actual graphics commands.
--
--   This preserves the sanctity of the GL type, and makes it so we don't ever
--   go full retard while parallelizing.
--
--   Currently, it only makes sense to call 'runGraphics' from the main thread.
--   Do not do it anywhere else.
module Graphics.Rendering.OpenGL.Monad.Unsafe ( GL
                                              , runGraphics
                                              , liftSTM
                                              , unsafeRunOnGraphicsCard
                                              ) where

import Prelewd

import IO

import Control.Concurrent.STM

-- | A wrapper for any actions performed on a graphics card. This allows us to
--   have a concurrent, yet easily thread-safe main game loop. Thank god for a
--   powerful type system.
newtype GL a = GL (SystemIO a)
    deriving (Functor, Applicative, Monad)

-- | Runs a set of graphics actions in the IO monad.
--
--   This action is thread-safe, and can therefore be used from multiple
--   threads. Every time we 'runGraphics', every graphical operation within is
--   atomic with respect to all other graphics being run.
runGraphics :: GL a -> SystemIO a
runGraphics (GL x) = x
{-# INLINE runGraphics #-}

-- | Runs an STM action inside the GL monad. This is allowed, since technically
--   no IO is being performed.
liftSTM :: STM a -> GL a
liftSTM = unsafeRunOnGraphicsCard . atomically
{-# INLINE liftSTM #-}

-- | Converts a graphics action in the IO monad into one in the GL monad.
--
--   Please, for the love of god, only use this on _actual_ graphics commands.
--   Anything else defeats the point entirely.
unsafeRunOnGraphicsCard :: SystemIO a -> GL a
unsafeRunOnGraphicsCard = GL
{-# INLINE unsafeRunOnGraphicsCard #-}

-- | Runs a computation inside a graphics monad indirectly embedded within
--   another graphics monad. This is for situations like:
--
--   @
--   UGLY.unsafeRunOnGraphicsCard . V.unsafeWith idata $ UGLY.runGraphics . \ptr ->
--      GL.texImage2D Nothing
--                    GL.NoProxy
--                    0
--                    intFmt
--                    (GL.TextureSize2D
--                       (fromIntegral width)
--                       (fromIntegral height))
--                    0
--                    $ GL.PixelData fmt GL.UnsignedByte ptr
--   @
--
--   Since we use an MVar to protect all OpenGL operations, this will prevent
--   a deadlock with the MVar being taken twice.

