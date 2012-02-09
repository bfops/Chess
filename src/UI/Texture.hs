{-# LANGUAGE RankNTypes #-}
-- | This module handles all things related to the loading and unloading of
--   Textures. The usual workflow with these is to grab an image from disk,
--   then upload it to graphics memory as needed.
--
--   Please don't forget to unload textures when done. If there's one thing
--   worse than a memory leak, it'd have to be a graphics memory leak.
module UI.Texture ( TextureHandle
                  , Texture(..)
                  , getImage
                  , uploadTexture
                  , uploadTextures
                  , unloadTexture
                  , unloadTextures
                  ) where

import           Codec.Picture                as Pic
import           Codec.PicturePrime           as PicPrime
import           Codec.Picture.Types          as PicTypes
import qualified Config
import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Vector.Storable         as V
import           Data.Word
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Paths_Chess                  as CP -- :)
import           System.FilePath
import           System.Log.Logger

-- | A handle to a texture in graphics memory.
type TextureHandle = GL.TextureObject

-- | A metadata wrapper around 'TextureHandle'. Generally, you will pass
--   'Texture' around internally, and use 'TextureHandle' when dealing with
--   OpenGL.
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

-- | OpenGL can't handle the JPEG color space without extensions, so just do it
--   in software if we every encounter it. Hopefully, this won't happen too
--   often since normal people don't use JPEG.
noJPEG :: DynamicImage -> DynamicImage
noJPEG (ImageYCbCr8 img) = ImageRGB8 $ convertImage img
noJPEG       img         = img

-- | Loads a texture from disk into memory, returning Nothing on
--   failure.
--
--   Usage:
--
--   > do tex <- getImage "yellow-dot.png"
getImage :: String -> IO (Maybe DynamicImage)
getImage name = do name' <- CP.getDataFileName $ Config.texturePrefix </> name
                   wrappedTex <- readImage' name'

                   case wrappedTex of
                       Left err  -> do infoM "UI.TextureCache" $
                                         "Could not load texture '" ++ name ++ "' from '" ++ name' ++ "': " ++ err
                                       return Nothing
                       Right tex -> return $ Just tex

-- | Uploads an image into OpenGL's graphics memory.
uploadTexture :: DynamicImage -> IO Texture
uploadTexture img = (head <$> GL.genObjectNames 1) >>= loadTexture' img

-- | Upoads multiple textures at once. Slightly more efficient than calling
--   'loadTexture' repeatedly.
uploadTextures :: [DynamicImage] -> IO [Texture]
uploadTextures xs = GL.genObjectNames (length xs)
                 >>= mapM (uncurry loadTexture') . zip xs

-- | Uploads a texture from memory into the graphics card. Internal
--   implementation.
loadTexture' :: DynamicImage -> TextureHandle -> IO Texture
loadTexture' tex handle = do let tex'        = noJPEG tex -- OpenGL can't handle YCrCb8.
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

-- | Removes a texture from OpenGL's graphics memory permenantly. Any further
--   uses of the texture for rendering results in undefined behavior.
--
--   TODO: Is there any way to enforce this shit with the type system? What
--         about ResourceT?
unloadTexture :: Texture -> IO ()
unloadTexture = unloadTextures . return
{-# INLINE unloadTexture #-}

-- | Removes a list of textures from OpenGL's graphics memory permenantly. Any
--   further uses of any of the textures for rendering results in undefined
--   behavior.
unloadTextures :: [Texture] -> IO ()
unloadTextures = GL.deleteObjectNames . map texHandle
{-# INLINE unloadTextures #-}

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

