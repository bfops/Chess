{-# LANGUAGE Rank2Types, MultiParamTypeClasses, DeriveDataTypeable #-}
-- | This module handles all things related to the loading and unloading of
--   Textures. The usual workflow with these is to grab an image from disk,
--   then upload it to graphics memory as needed.
--
--   Textures are automatically deallocated by the garbage collector so you
--   don't need to think about resource allocation, however, this is quite
--   experimental. One day, I'll verify this is working.
module Game.Resource.Texture ( Texture( texWidth, texHeight )
                             , DynamicImage
                             , getImage
                             , uploadTexture
                             , uploadTextures
                             , renderTexture
                             -- * Low level image functions.
                             , imageWidth'
                             , imageHeight'
                             ) where

import           Codec.Picture                   as Pic
import           Codec.Picture.Types             as PicTypes
import qualified Config
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Data.Global
import qualified Data.Text                       as T
import           Data.Typeable
import qualified Data.Vector.Storable            as V
import           Data.Word
import           Foreign ( ForeignPtr, touchForeignPtr )
import           Foreign.Concurrent
import           Foreign.Ptr
import           Game.Resource.Loadable
import qualified Graphics.Rendering.OpenGL.Monad as GL
import qualified Graphics.Rendering.OpenGL.Monad.Unsafe as UGLY
import           System.FilePath
import           System.Log.Logger

newtype TextureHandle = THandle GL.TextureObject
    deriving (Show, Eq, Ord, Typeable)

unHandle :: TextureHandle -> GL.TextureObject
unHandle (THandle x) = x
{-# INLINE unHandle #-}

-- | A metadata wrapper around an OpenGL 'TextureObject'. Generally, you will
--   pass 'Texture' around internally, and use texHandle when dealing
--   with OpenGL.
data Texture = Texture { texWidth  :: !Int -- ^ Width in pixels.
                       , texHeight :: !Int -- ^ Height in pixels.
                       -- | A handle to the texture in graphics memory.
                       --   This handle is INVALID if the parent texture
                       --   becomes unreachable. Therefore, never store
                       --   a copy of it - just access it as you need it.
                       , texHandle :: !TextureHandle
                       -- | By attaching a foreign pointer to the texture,
                       --   we can add a finalizer to release it when all
                       --   references are lost.
                       , texPtr :: {-# UNPACK #-} !(ForeignPtr ())
                       }
    deriving (Show, Eq, Ord, Typeable)

instance NFData Texture

instance LoadableResource DynamicImage Texture where
    fromDisk   = getImage
    toGraphics = uploadTextures

-- The whole texture allocation scheme here is just plain crafty (and also a
-- potential source for a huge number of bugs.
--
-- We keep a global list (thread-safe, of course) of all textures awaiting
-- "cleanup". These are textures which have no more references to them and need
-- to have GL.deleteObjectNames run on them.
--
-- Before we allocate a new texture, we always scavenge this list, cleaning
-- up any expired textures. This ensures that we always have free handles
-- when we need them.
--
-- Finally, when allocating our texture, we throw in an unpacked foreign
-- pointer, with a finalizer which adds the texture to the 'toFinalize' list.
-- Hopefully (this has not been confirmed), this finalizer will run soon after
-- it becomes unreachable (aka, when the parent Texture becomes unreachable),
-- and queue itself for deletion. Next time we need a new texture handle,
-- we will have a whole bunch of freshly freed ones.
--
-- Note that all finalizers will be run in a seperate thread, so toFinalize
-- MUST be a concurrent data structure.
--
-- We don't need to be timely with resource cleanup, since OpenGL will use an
-- LRU cache to expire the oldest textures to RAM when it runs out of room.
-- Also, we generally don't allocate and deallocate textures very frequently,
-- and most certainly not every frame.

-- | A list of textures awaiting finalization.
--   Yes, this is a global variable. Deal with it.
toFinalize :: MVar [TextureHandle]
toFinalize = declareMVar "Game.Texture.toFinalize" []

-- | Runs the finalizers of anything in the toFinalize list.
cleanupFinalizers :: IO ()
cleanupFinalizers = modifyMVar_ toFinalize (\x -> freeTex x >> return [])
    where
        freeTex = GL.runGraphics . GL.deleteObjectNames . map unHandle

-- | Allocates a new texture, associating a finalizer with it so OpenGL cleans
--   its shit up.
newTexture :: Int -> Int -> TextureHandle -> GL.GL Texture
newTexture width height handle = UGLY.unsafeRunOnGraphicsCard cleanupFinalizers
                              >>  UGLY.unsafeRunOnGraphicsCard (newForeignPtr nullPtr final)
                              >>= return . Texture width height handle
    where
        -- When we finalize a texture, just add it to the toFinalize list.
        -- We'll actually do the free-ing next time around, in cleanupFinalizers.
        final = modifyMVar_ toFinalize (return . (handle:))
{-# ANN newTexture "HLint: ignore" #-}

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
getImage :: T.Text -> IO (Maybe DynamicImage)
getImage name = do let name' = Config.texturePrefix </> T.unpack name
                   wrappedTex <- readImage name'

                   case wrappedTex of
                       Left err  -> do infoM "UI.TextureCache" $
                                         "Could not load texture '" ++ T.unpack name ++ "' from '" ++ name' ++ "': " ++ err
                                       return Nothing
                       Right tex -> return $ Just tex

-- | Uploads an image into OpenGL's graphics memory.
uploadTexture :: DynamicImage -> GL.GL Texture
uploadTexture img = ((THandle . head) <$> GL.genObjectNames 1) >>= loadTexture' img

-- | Upoads multiple textures at once. Slightly more efficient than calling
--   'loadTexture' repeatedly.
uploadTextures :: [DynamicImage] -> GL.GL [Texture]
uploadTextures xs = (map THandle <$> GL.genObjectNames (length xs))
                 >>= mapM (uncurry loadTexture') . zip xs

-- | Uploads a texture from memory into the graphics card. Internal
--   implementation.
loadTexture' :: DynamicImage -> TextureHandle -> GL.GL Texture
loadTexture' tex handle = do let tex'        = noJPEG tex -- OpenGL can't handle YCrCb8.
                                 width       = imageWidth' tex'
                                 height      = imageHeight' tex'
                                 idata       = imageData' tex'
                                 intFmt      = internalPixelFormat tex'
                                 fmt         = pixelFormat tex'

                             GL.glDebugM "Game.Texture.loadTexture'"
                                $ "Uploading a " ++ show width ++ "x" ++ show height ++ " texture into object " ++ show handle

                             GL.textureBinding GL.Texture2D GL.$= Just (unHandle handle)

                             -- Since we don't modify the actual image data, we
                             -- can avoid a copy (unsafeWith), and run it on the
                             -- graphics card as if it were a graphics command
                             -- (unsafeRunOnGraphicsCard).
                             UGLY.unsafeRunOnGraphicsCard . V.unsafeWith idata $ UGLY.runGraphics . \ptr ->
                                GL.texImage2D Nothing
                                              GL.NoProxy
                                              0
                                              intFmt
                                              (GL.TextureSize2D
                                                (fromIntegral width)
                                                (fromIntegral height))
                                              0
                                              $ GL.PixelData fmt GL.UnsignedByte ptr

                             newTexture width height handle

-- | Draws a texture onto the screen, with position, rotation, scale, etc.
--   determined by the current OpenGL state.
renderTexture :: Texture -> GL.GL ()
renderTexture tex = do GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Repeated, GL.Repeat)
                       GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Repeated, GL.Repeat)
                       GL.textureFilter   GL.Texture2D      GL.$= ((GL.Nearest, Nothing), GL.Nearest)
                       -- Do nearest neighbor interpolation.

                       -- Enable texturing.
                       GL.texture GL.Texture2D GL.$= GL.Enabled
                       GL.textureFunction      GL.$= GL.Combine

                       -- Set current texture.
                       GL.textureBinding GL.Texture2D GL.$= Just (unHandle $ texHandle tex)

                       -- Set to opaque.
                       GL.currentColor GL.$= GL.Color4 1.0 1.0 1.0 1.0

                       -- Blam! Draw that textured square. We must move clockwise
                       -- from the top left of the image, so sayeth OpenGL.
                       GL.renderPrimitive GL.Polygon $ do GL.texCoord' 0 0; GL.vertex' left top;
                                                          GL.texCoord' 1 0; GL.vertex' right top;
                                                          GL.texCoord' 1 1; GL.vertex' right bottom;
                                                          GL.texCoord' 0 1; GL.vertex' left bottom;

                       GL.texture GL.Texture2D GL.$= GL.Disabled

                       -- Ensures the texture will never be collected before
                       -- this point.
                       UGLY.unsafeRunOnGraphicsCard . touchForeignPtr $ texPtr tex
    where
        left = 0
        right = texWidth tex
        top = texHeight tex
        bottom = 0


-- | Lets us pass a function "through" a dynamic image, ignoring its type and
--   getting right down to the raw data.
dynToStaticImage :: (forall a. Image a -> b) -> DynamicImage -> b
dynToStaticImage f (ImageY8     img) = f img
dynToStaticImage f (ImageYA8    img) = f img
dynToStaticImage f (ImageRGB8   img) = f img
dynToStaticImage f (ImageRGBA8  img) = f img
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
