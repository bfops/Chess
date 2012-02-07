{-# LANGUAGE RankNTypes #-}
module UI.TextureCache ( Texture(..)
                       , TextureCache
                       , TextureHandle
                       , newTextureCache
                       , loadTexture
                       , clearCache
                       ) where

import           Codec.Picture                as Pic
import           Codec.Picture.Types          as PicTypes
import           Data.IORef
import           Data.StateVar
import qualified Data.Vector.Storable         as V
import           Data.Word
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Paths_Chess                  as CP -- :)
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

-- | A texture cache is used to keep track of all loaded textures. This prevents
--   texture duplication in graphics memory, and allows us to clean them all up
--   at the end with 'clear'.
newtype TextureCache = TextureCache (IORef [(String, Texture)])

-- | Creates a new, empty texture cache. To begin using it, just call 'loadTexture'!
newTextureCache :: IO TextureCache
newTextureCache = do r <- newIORef []
                     return $ TextureCache r

-- | Loads a texture, either from disk or the cache. Obviously, the cache will
--   be preferred. If this function returns Nothing, the image could not be
--   loaded. This is most likely due to the file not existing, or being corrupt.
--   All errors will be logged instead of thrown.
loadTexture :: TextureCache -> String -> IO (Maybe Texture)
loadTexture (TextureCache tc) name =
        do cache <- readIORef tc
           case lookup name cache of
               Just tex' -> do debugM "UI.TextureCache" ("Texture cache hit: " ++ name)
                               return $ Just tex'
               Nothing -> do infoM "UI.TextureCache" ("Texture cache miss: " ++ name)
                             tex <- loadTexFromDisk name
                             case tex of
                                Nothing -> return Nothing
                                Just tex' -> do writeIORef tc ((name, tex'):cache)
                                                return $ Just tex'

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

-- | OpenGL can't handle the JPEG color space without extensions, so just do it
--   in software if we every encounter it. Hopefully, this won't happen too
--   often since normal people don't use JPEG.
noJPEG :: DynamicImage -> DynamicImage
noJPEG (ImageYCbCr8 img) = ImageRGB8 $ convertImage img
noJPEG       img         = img

-- | Loads a texture from disk into graphics memory, returning Nothing on
--   failure.
loadTexFromDisk :: String -> IO (Maybe Texture)
loadTexFromDisk name = do name' <- CP.getDataFileName name
                          wrappedTex <- readImage name'

                          case wrappedTex of
                              Left err  -> do infoM "UI.TextureCache" $
                                                "Could not load texture '" ++ name ++ "' from '" ++ name' ++ "': " ++ err
                                              return Nothing
                              Right tex -> do tex' <- uploadTexToGraphicsCard tex
                                              return $ Just tex'

-- | Uploads a texture from memory into the graphics card.
uploadTexToGraphicsCard :: DynamicImage -> IO Texture
uploadTexToGraphicsCard tex = do let tex'        = noJPEG tex -- OpenGL can't handle YCrCb8.
                                     width       = imageWidth' tex'
                                     height      = imageHeight' tex'
                                     idata       = imageData' tex'
                                     intFmt      = internalPixelFormat tex'
                                     fmt         = pixelFormat tex'

                                 [handle] <- GL.genObjectNames 1
                                 GL.textureBinding GL.Texture2D $= Just handle

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

                                 return $ Texture { texWidth = width
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