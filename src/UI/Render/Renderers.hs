{-# LANGUAGE RankNTypes #-}
module UI.Render.Renderers ( rectangleRenderer
                           , textureRenderer
                           , textRenderer
                           ) where

import Codec.Picture
import Codec.Picture.Types
import Data.Array.Storable
import Data.Array.Unboxed
import Data.Word
import qualified Graphics.UI.GLUT as GLUT
import System.Log.Logger
import UI.Render.Core

-- | This data structure is temporary. Eventually, this will just be a handle
--   to the right texture id on the graphics card.
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
--   > coinTex <- textureRenderer "coin.png"
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

-- | A simple demo renderer for a rectangle.
rectangleRenderer :: GLUT.Color a
                  =>  Int -- ^ The width of the desired rectangle.
                  -> Int  -- ^ The height of the desired rectangle.
                  -> a    -- ^ The color of the desired rectangle.
                  -> Renderer
rectangleRenderer width height color = defaultRenderer { render = rectRender
                                                       , dims = (width, height)
                                                       }
    where
        rectRender :: IO ()
        rectRender = do GLUT.color color
                        let vertex3f = GLUT.vertex :: GLUT.Vertex3 GLUT.GLfloat -> IO ()
                        GLUT.renderPrimitive GLUT.Polygon $ mapM_ vertex3f
                            [ GLUT.Vertex3 left bottom 0.0
                            , GLUT.Vertex3 right bottom 0.0
                            , GLUT.Vertex3 right top 0.0
                            , GLUT.Vertex3 left top 0.0
                            ]
        top = fromIntegral height
        bottom = 0.0
        left = 0.0
        right = fromIntegral width
