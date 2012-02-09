{-# LANGUAGE TupleSections #-}
module UI.TextureCache ( Texture(..)
                       , TextureCache
                       , TextureHandle
                       , newTextureCache
                       , loadTexture
                       , preloadTextures
                       , clearCache
                       ) where

import           Codec.Picture                as Pic
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class ( liftIO )
import           Data.Conduit                 as Cond
import qualified Data.Conduit.List            as C
import           Data.Conduit.TMChan
import           Data.IORef
import           Data.List
import           Data.Maybe
import           UI.Texture
import           System.Log.Logger

-- | A texture cache is used to keep track of all loaded textures. This prevents
--   texture duplication in graphics memory, and allows us to clean them all up
--   at the end with 'clear'.
--
--   TODO: Would a different data structure be more appropriate here? It seems
--         like overkill, but that may change in the future.
newtype TextureCache = TextureCache (IORef [(String, Texture)])

-- | Creates a new, empty texture cache. To begin using it, just call 'loadTexture'!
newTextureCache :: IO TextureCache
newTextureCache = TextureCache <$> newIORef []

-- | Loads a texture, either from disk or the cache. Obviously, the cache will
--   be preferred. If this function returns Nothing, the image could not be
--   loaded. This is most likely due to the file not existing, or being corrupt.
--   All errors will be logged instead of thrown.
loadTexture :: TextureCache -> String -> IO (Maybe Texture)
loadTexture (TextureCache tc) name =
        do cache <- readIORef tc
           case lookup name cache of
               Just tex -> do debugM "UI.TextureCache" ("Texture cache hit: " ++ name)
                              return $ Just tex
               Nothing -> do infoM "UI.TextureCache" ("Texture cache miss: " ++ name)
                             img <- getImage name
                             case img of
                                Nothing -> return Nothing
                                Just img' -> do tex <- uploadTexture img'
                                                writeIORef tc $ (name, tex):cache
                                                return $ Just tex

-- | Preloads a list of textures into a texture cache. Invalid textures will be
--   silently ignored.
--   
--   This is more efficient than calling @mapM_ (loadTexture tc)@ because it will
--   load textures from disk and load textures into graphics memory concurrently.
preloadTextures :: TextureCache -> [String] -> IO ()
preloadTextures (TextureCache tc) texts =
        do cache <- readIORef tc

           -- We only load uncached items. We detect these by getting all unique
           -- names, in the new cache, then removing duplicates, and those that
           -- appeared in the old cache.
           let cachedNames = map fst cache
               notCached   = (nub . sort $ texts ++ cachedNames) \\ cachedNames

           chan <- atomically newTMChan

           -- Load all textures from disk in a new thread.
           _ <- forkIO . runResourceT $ C.sourceList notCached $$ diskLoaderConduit =$ sinkTMChan chan

           -- Grab the disk textures one-by-one and throw it into graphics mem.
           -- Note that we have to run diskToGraphics in _this_ thread, because
           -- OpenGL is very single-threaded.
           runResourceT $ sourceTMChan chan $$ toGraphicsMem
    where
        -- Dumps the images in the conduit into graphics memory, caching all
        -- the handles as we go along.
        toGraphicsMem :: Sink (String, DynamicImage) IO ()
        toGraphicsMem = SinkData push close
            where
                -- Texture loaded from disk. Dump into graphics mem and the cache.
                push (name, img) = do tex    <- liftIO $ uploadTexture img
                                      cache  <- liftIO $ readIORef tc
                                      liftIO . writeIORef tc $ (name, tex):cache
                                      return $ Processing push close
                close = return ()

-- Converts texture names into loaded textures, zipped with their
-- original name.
diskLoaderConduit :: Conduit String IO (String, DynamicImage)
diskLoaderConduit = C.mapM (\name -> (name,) <$> getImage name) -- Load the textures, keep the name.
                  -- Replace the (String, Maybe ..) with Maybe (String, ...)
                 =$= C.map (\(name, tex) -> (name,) <$> tex)
                  -- Only keep successfully loaded textures.
                 =$= C.filter isJust =$= C.map fromJust
                  -- Make sure to fully evaluate it before sending it off to
                  -- the other thread.
                 =$= C.map force

-- | Empties the texture cache, deleting all cached textures from graphics
--   memory. Do not add textures to multiple caches and then call clear on one
--   or both of them. This leads to Very Bad Things. If you want to use multiple
--   caches, make sure the textures in them are distinct.
--
--   You may continue to use the cache after calling this function - it will
--   just be empty.
clearCache :: TextureCache -> IO ()
clearCache (TextureCache tc) = do cache <- readIORef tc
                                  unloadTextures $ map snd cache
                                  writeIORef tc []
