module Game.Loaders ( Loaders(..)
                    , updateLoaders
                    , runLoadersDeferred
                    ) where

import Control.Applicative

import Game.Resource.Loader
import Game.Resource.Texture

import Graphics.Rendering.OpenGL.Monad

-- | The resource loaders associated with any running game.
data Loaders = Loaders { textureL :: ResourceLoader DynamicImage Texture
                       }

-- | Runs 'chooseResources' on all available loaders.
--   
--   Adding a loader? Applicative its 'chooseResource' function on the end here.
--
--   Do we have a whole bunch of loaders? Run the resource choosers in parallel!
updateLoaders :: Loaders -> [ResourceRequest] -> IO Loaders
updateLoaders l rs = Loaders <$> chooseResources (textureL l) rs

runLoadersDeferred :: Loaders -> GL Loaders
runLoadersDeferred l = Loaders <$> runDeferred (textureL l)
