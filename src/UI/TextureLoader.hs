{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}
module UI.TextureLoader ( Texture(..)
                        , TextureLoader
                        , emptyTextureLoader
                        , chooseTextures
                        , getTexture
                        ) where

import           Codec.Picture
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMChan
import           Control.DeepSeq
import           Data.Conduit                 as Cond
import qualified Data.Conduit.List            as C
import           Data.Conduit.TMChan
import           Data.HashMap                 as M
import           Data.Maybe
import           Graphics.Rendering.OpenGL.Monad
import           Util.HashString
import           UI.Texture

-- | The texture loader keeps track of all textures loaded on the graphics
--   card.
newtype TextureLoader = TextureLoader (M.Map HashString Texture)
    deriving NFData

-- | Create a new texture loaded, with no textures queued.
emptyTextureLoader :: TextureLoader
emptyTextureLoader = TextureLoader M.empty

-- | Ensures that the given list of textures is loaded. If a loaded texture
--   isn't in the list, it will be unloaded. The texture names are given as
--   Text (not String), so make sure to use the OverloadedStrings extension.
--
--   This function will block until all the textures are successfully loaded.
chooseTextures :: TextureLoader -- ^ The loader we'll consult for already
                               --   loaded textures so we can avoid a double
                               --   allocation. It's okay if this is
                               --   'emptyTextureLoader'.
               -> [HashString] -- ^ A list of textures that will be required in the
                              --   next render iteration.
               -> IO TextureLoader
chooseTextures (TextureLoader texMap) names = let (toLoad, alreadyLoaded) = checkLoaded
                                               in do texs <- loadTexes toLoad
                                                     return $!! TextureLoader $ foldr (\(name, img) m -> M.insert name img m) alreadyLoaded texs
    where
        -- Checks if the list of textures is loaded in the texMap. Those that
        -- aren't are stored in the left side of the tuple, and those that are,
        -- are in the right side.
        checkLoaded :: ([HashString], M.Map HashString Texture)
        checkLoaded = foldr sortLoaded ([], M.empty) names

        -- Takes a name and an existing (loaded, unloaded) pair and returns a
        -- new (loaded, unloaded) pair depending on the [un]loaded status of
        -- then texture with the given name.
        sortLoaded :: HashString -> ([HashString], M.Map HashString Texture)
                                -> ([HashString], M.Map HashString Texture)
        sortLoaded name (unloaded, loaded) = case M.lookup name texMap of
                                               Nothing -> (name:unloaded, loaded)
                                               Just tex -> (unloaded, M.insert name tex loaded)

-- | Gets a texture from the loader. If Nothing is returned, then either the
--   texture does not exist, or it has not been loaded yet.
getTexture :: TextureLoader -> HashString -> Maybe Texture
getTexture (TextureLoader texMap) name = M.lookup name texMap

-- | Loads a list of textures by name, returning all successfully loaded
--   textures in a list, zipped with its name.
loadTexes :: [HashString] -> IO [(HashString, Texture)]
loadTexes names = do chan <- atomically newTMChan

                     -- Load all textures from disk in a new thread.
                     _ <- forkIO . runResourceT $ C.sourceList names
                                                 $$ diskLoaderConduit
                                                 =$ sinkTMChan chan

                     -- Grab the disk textures one by one and throw them into
                     -- graphics memory. This lets us interleave texture
                     -- loading with normal renderer operation.
                     runResourceT $ sourceTMChan chan
                                    $$ C.mapM toGraphicsMem
                                    =$ C.consume

-- | Uploads a (name, imageInMemory) pair, and returns a new pair:
--   (name, imageInGraphicsMemory).
toGraphicsMem :: (HashString, DynamicImage) -> IO (HashString, Texture)
toGraphicsMem (name, img) = (name,) <$> runGraphics (uploadTexture img)

-- | Converts texture names into loaded textures, zipped with their
--   original name.
diskLoaderConduit :: Conduit HashString IO (HashString, DynamicImage)
diskLoaderConduit = C.mapM (\name -> (name,) <$> getImage (fromHashString name)) -- Load the textures, keep the name.
                  -- Replace the (Text, Maybe ..) with Maybe (Text, ...)
                 =$= C.map (\(name, tex) -> (name,) <$> tex)
                  -- Only keep successfully loaded textures.
                 =$= C.filter isJust =$= C.map fromJust
                  -- Make sure to fully evaluate it before sending it off to
                  -- the other thread.
                 =$= C.map force
