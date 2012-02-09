{-# LANGUAGE RankNTypes, TupleSections #-}
module UI.TextureCache ( Texture(..)
                       , TextureCache
                       , TextureHandle
                       , newTextureCache
                       , loadTexture
                       , preloadTextures
                       , clearCache
                       ) where

import           Codec.Picture                as Pic
import           Codec.PicturePrime           as PicPrime
import           Codec.Picture.Types          as PicTypes
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
import qualified Data.Vector.Storable         as V
import           Data.Word
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Paths_Chess                  as CP -- :)
import           System.FilePath
import           System.Log.Logger

-- | A handle to our texture in graphics memory.
type TextureHandle = GL.TextureObject

-- | This data structure is temporary. Eventually, this will just be a handle
--   to the right texture id on the graphics card.
data Texture = Texture { texWidth  :: Int -- ^ Width in pixels.
                       , texHeight :: Int -- ^ Height in pixels.
                       -- | A handle to the texture in graphics memory.
                       , texHandle :: TextureHandle
                       }

instance NFData Texture where
    rnf (Texture width height hand) = rnf width  `seq`
                                      rnf height `seq`
                                      hand       `seq`
                                      ()

-- | A texture cache is used to keep track of all loaded textures. This prevents
--   texture duplication in graphics memory, and allows us to clean them all up
--   at the end with 'clear'.
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
                             tex <- loadTexFromDisk name
                             case tex of
                                Nothing -> return Nothing
                                Just tex' -> do [handle] <- GL.genObjectNames 1
                                                tex'' <- uploadTexToGraphicsCard handle tex'
                                                writeIORef tc $ (name, tex''):cache
                                                return $ Just tex''

-- | Temporary, should be able to be removed with the next version of conduit.
--   Patch pending.
conduitZip :: Monad m => [b] -> Conduit a m (a, b)
conduitZip   []   = Conduit emptyPush close
    where
        emptyPush x = return $ Finished (Just x) []
        close = return []
conduitZip (y:ys) = Conduit push close
    where
        push x = return $ Producing (conduitZip ys) [(x, y)]
        close  = return []

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
           _ <- forkIO . runResourceT $ C.sourceList notCached Cond.$= diskLoaderConduit $$ sinkTMChan chan

           -- Get valid handles for all our to-be-loaded textures.
           handles <- GL.genObjectNames $ length notCached

           -- Grab the disk textures one-by-one and throw it into graphics mem.
           -- Note that we have to run diskToGraphics in _this_ thread, because
           -- OpenGL is very single-threaded.
           runResourceT $ sourceTMChan chan Cond.$= conduitZip handles Cond.$$ toGraphicsMem
    where
        -- Dumps the images in the conduit into graphics memory, caching all
        -- the handles as we go along.
        toGraphicsMem :: Sink (Maybe (String, DynamicImage), TextureHandle) IO ()
        toGraphicsMem = sink []
            where
                -- toFree is a list of textures we need to free.
                sink toFree = SinkData (push toFree) (close toFree)
                -- Texture loaded from disk. Dump into graphics mem and the cache.
                push toFree (Just (name, tex), handle) = do tex' <- liftIO $ uploadTexToGraphicsCard handle tex
                                                            cache <- liftIO $ readIORef tc
                                                            liftIO . writeIORef tc $ (name, tex'):cache
                                                            return $ Processing (push toFree) (close toFree)
                push toFree (Nothing, handle) = let toFree' = handle:toFree
                                                 in return $ Processing (push toFree') (close toFree')
                close toFree = liftIO $ GL.deleteObjectNames toFree

-- Converts texture names into loaded textures, zipped with their
-- original name.
diskLoaderConduit :: Conduit String IO (Maybe (String, DynamicImage))
diskLoaderConduit = C.mapM (\name -> (name,) <$> loadTexFromDisk name) -- Load the textures, keep the name.
                  -- Replace the (String, Maybe ..) with Maybe (String, ...)
                 =$= C.map (\(name, tex) -> (name,) <$> tex)

-- | OpenGL can't handle the JPEG color space without extensions, so just do it
--   in software if we every encounter it. Hopefully, this won't happen too
--   often since normal people don't use JPEG.
noJPEG :: DynamicImage -> DynamicImage
noJPEG (ImageYCbCr8 img) = ImageRGB8 $ convertImage img
noJPEG       img         = img

-- | Loads a texture from disk into memory, returning Nothing on
--   failure.
loadTexFromDisk :: String -> IO (Maybe DynamicImage)
loadTexFromDisk name = do name' <- CP.getDataFileName $ "assets" </> name
                          wrappedTex <- readImage' name'

                          case wrappedTex of
                              Left err  -> do infoM "UI.TextureCache" $
                                                "Could not load texture '" ++ name ++ "' from '" ++ name' ++ "': " ++ err
                                              return Nothing
                              Right tex -> return $ Just tex

-- | Uploads a texture from memory into the graphics card.
uploadTexToGraphicsCard :: TextureHandle -> DynamicImage -> IO Texture
uploadTexToGraphicsCard handle tex = do let tex'        = noJPEG tex -- OpenGL can't handle YCrCb8.
                                            width       = imageWidth' tex'
                                            height      = imageHeight' tex'
                                            idata       = imageData' tex'
                                            intFmt      = internalPixelFormat tex'
                                            fmt         = pixelFormat tex'

                                        GL.textureBinding GL.Texture2D GL.$= Just handle

                                        V.unsafeWith idata $ \ptr ->
                                            GL.texImage2D Nothing
                                                        GL.NoProxy
                                                        0
                                                        intFmt
                                                        (GL.TextureSize2D
                                                            (fromIntegral width)
                                                            (fromIntegral height))
                                                        0
                                                        $ GL.PixelData fmt GL.UnsignedByte ptr

                                        return Texture { texWidth = width
                                                       , texHeight = height
                                                       , texHandle = handle
                                                       }

-- | Empties the texture cache, deleting all cached textures from graphics
--   memory. Do not add textures to multiple caches and then call clear on one
--   or both of them. This leads to Very Bad Things. If you want to use multiple
--   caches, make sure the textures in them are distinct.
--
--   You may continue to use the cache after calling this function - it will
--   just be empty.
clearCache :: TextureCache -> IO ()
clearCache (TextureCache tc) = do cache <- readIORef tc
                                  GL.deleteObjectNames $ map (texHandle . snd) cache
                                  writeIORef tc []

-- | Lets us pass a function "through" a dynamic image, ignoring its type and
--   getting right down to the raw data.
dynToStaticImage :: (forall a. Image a -> b) -> DynamicImage -> b
dynToStaticImage f (ImageY8 img)     = f img
dynToStaticImage f (ImageYA8 img)    = f img
dynToStaticImage f (ImageRGB8 img)   = f img
dynToStaticImage f (ImageRGBA8 img)  = f img
dynToStaticImage f (ImageYCbCr8 img) = f img

-- | Gets a dynamic image's width.
imageWidth' :: DynamicImage -> Int
imageWidth' = dynToStaticImage PicTypes.imageWidth

-- | Gets a dynamic image's height.
imageHeight' :: DynamicImage -> Int
imageHeight' = dynToStaticImage PicTypes.imageHeight

-- | Gets the raw bytes associated with a dynamic image.
imageData' :: DynamicImage -> V.Vector Word8
imageData' = dynToStaticImage imageData

-- | Parses the image's type and returns the appropriate OpenGL internal pixel
--   format.
internalPixelFormat :: DynamicImage -> GL.PixelInternalFormat
internalPixelFormat (ImageY8 _)     = GL.Luminance8
internalPixelFormat (ImageYA8 _)    = GL.Luminance8Alpha8
internalPixelFormat (ImageRGB8 _)   = GL.RGB8
internalPixelFormat (ImageRGBA8 _)  = GL.RGBA8
internalPixelFormat (ImageYCbCr8 _) = error "OpenGL does not support YCbCr images. Convert it to RGB8 first."

-- | Parses the image's type and returns the appropriate OpenGL pixel format.
pixelFormat :: DynamicImage -> GL.PixelFormat
pixelFormat (ImageY8 _)     = GL.Luminance
pixelFormat (ImageYA8 _)    = GL.LuminanceAlpha
pixelFormat (ImageRGB8 _)   = GL.RGB
pixelFormat (ImageRGBA8 _)  = GL.RGBA
pixelFormat (ImageYCbCr8 _) = GL.YCBCR422

-- TODO: The following wrappers should be removed once the next version of
--       JuicyPixels is released. I have already submitted a patch to this
--       effect.
--
-- Need deepseq instances for JuicyPixels, along with catching IO exceptions.
