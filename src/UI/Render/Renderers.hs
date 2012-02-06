{-# LANGUAGE RankNTypes #-}
module UI.Render.Renderers ( textureRenderer
                           , textRenderer
                           ) where

import Codec.Picture
import Codec.Picture.Types
import Data.Array.Storable
import Data.Array.Unboxed
import Data.Word
import System.Log.Logger
import UI.Render.Core

data RenderableImage = RenderableImage { rImageWidth  :: Int
                                       , rImageHeight :: Int
                                       , rImageData   :: StorableArray Int Word8
                                       }

renderTexture :: RenderableImage -> IO ()
renderTexture _ = return ()

dynToStaticImage :: (forall a. Image a -> b) -> DynamicImage -> b
dynToStaticImage f (ImageY8 img)     = f img
dynToStaticImage f (ImageYA8 img)    = f img
dynToStaticImage f (ImageRGB8 img)   = f img
dynToStaticImage f (ImageRGBA8 img)  = f img
dynToStaticImage f (ImageYCbCr8 img) = f img

imageWidth' :: DynamicImage -> Int
imageWidth' = dynToStaticImage imageWidth

imageHeight' :: DynamicImage -> Int
imageHeight' = dynToStaticImage imageHeight

imageData' :: DynamicImage -> UArray Int Word8
imageData' = dynToStaticImage imageData

loadImage :: String -> IO RenderableImage
loadImage texName = do wrappedTex <- readImage texName
                       case wrappedTex of
                            Left err -> errorM "UI.Render.TextureRenderer" ("Could not load texture '" ++ texName ++ "': " ++ err)
                                     >> loadImage "texture-not-found.png"
                            Right rawTex -> makeRenderable rawTex
    where
        makeRenderable :: DynamicImage -> IO RenderableImage
        makeRenderable img = do stArray <- unsafeThaw $ imageData' img
                                return RenderableImage { rImageWidth = imageWidth' img
                                                       , rImageHeight = imageHeight' img
                                                       , rImageData = stArray
                                                       }

renderText :: String -> String -> IO ()
renderText _ _ = undefined

-- | Loads a texture, and returns a new renderer for it.
--
--   Usage:
--
--   > coinTex <- textureRenderer "coin"
--   > updateWindow w $ coinTex' { vAlign = VCenterAlign 0
--   >                           , hAlign = HCenterAlign 0
--   >                           , children = getCoinChildren
--   >                           }
textureRenderer :: String -> IO Renderer
textureRenderer name = do rawTex <- loadImage name
                          return $ defaultRenderer { render = renderTexture rawTex
                                                   , dims   = (rImageWidth rawTex, rImageHeight rawTex)
                                                   }

textRenderer :: String -- ^ The name of the font we will use for rendering.
             -> String -- ^ The string we're drawing.
             -> Renderer
textRenderer fontName label = defaultRenderer { render = renderText fontName label
                                              }
