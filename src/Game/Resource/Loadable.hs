{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Game.Resource.Loadable ( LoadableResource(..)
                              ) where

import Data.Text
import Graphics.Rendering.OpenGL.Monad ( GL )

-- | A 'LoadableResource' is any resource which can be loaded from disk, that
--   we'll need to access in renderers.
--
--   If you need to upload a resource (such as a texture) to the graphics card,
--   do that in 'toGraphics'. Otherwise, keep all disk-loading in 'fromDisk'.
--
--   'i' is the intermediate, in-memory resource type. 'r' is the uploaded,
--   in-vram type. If you don't need to have your resource uploaded to the
--   graphics card, make 'i' and 'r' the same type, and 'toGraphics' a no-op.
class LoadableResource i r | i -> r, r -> i where
    -- | Loads a resource (with the given name) from disk, and into an
    --   intermediate RAM-only buffer. This buffer can be of any type, denoted
    --   by 'IntermediateRepr'.
    fromDisk   :: Text -> IO (Maybe i)

    -- | Uploads a RAM-loaded resource onto the graphics card. If no such
    --   upload is needed, set the body of this function to @return@,
    --   and make sure that 'i' is the same as 'r'.
    toGraphics :: [i] -> GL [r]

