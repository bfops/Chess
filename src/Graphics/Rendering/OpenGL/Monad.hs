{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Defines the OpenGL monad. All actions dealing with the graphics card must
--   be wrapped in this monad. This helps us ensure silly things like them all
--   running in the same thread, and avoiding the interleaving of random IO.
module Graphics.Rendering.OpenGL.Monad ( GL
                                       , runGraphics
                                       , module Graphics.Rendering.OpenGL.Monad.Wrappers
                                       ) where

import Graphics.Rendering.OpenGL.Monad.Unsafe
import Graphics.Rendering.OpenGL.Monad.Wrappers
