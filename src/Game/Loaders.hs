-- | Keeps all game loaders in one record, for easy access from various parts
--   of the game.
module Game.Loaders ( Loaders(..)
                    , updateLoaders
                    , runLoadersDeferred
                    ) where

import Prelewd

import IO

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
updateLoaders :: Loaders -> [ResourceRequest] -> SystemIO Loaders
updateLoaders = (Loaders <$>)<$$> chooseResources . textureL

-- | Calls 'runDeferred' on all loaders.
--
--   Adding a loader? Applicative its 'runDeferred' function on the end here.
runLoadersDeferred :: Loaders -> GL Loaders
runLoadersDeferred = Loaders <$$> runDeferred . textureL
