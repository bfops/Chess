{-# LANGUAGE RankNTypes #-}
module UI.Render.Renderers ( rectangleRenderer
                           , textureRenderer
                           , textRenderer
                           ) where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.UI.GLUT as GLUT
import UI.Colors
import UI.TextureCache
import UI.Render.Core

-- | Sends the draw command for one vertex. Meant to be used in renderPrimitive
--   and its ilk.
vertex' :: Int -> Int -> IO ()
vertex' x y = GL.vertex $ (GL.Vertex2 :: GLdouble
                                      -> GLdouble
                                      -> GL.Vertex2 GLUT.GLdouble)
                              (fromIntegral x)
                              (fromIntegral y)

-- | Just like 'vertex', except for a texture coordinate.
--
--   See: 'vertex'
texCoord' :: Int -> Int -> IO ()
texCoord' x y = GL.texCoord $ (GL.TexCoord2 :: GLdouble
                                            -> GLdouble
                                            -> GL.TexCoord2 GLdouble)
                                  (fromIntegral x)
                                  (fromIntegral y)

-- | The width and height of the "no texture" box.
noTexDims :: Dimensions
noTexDims = (100, 100)

renderTexture :: Maybe Texture -> IO ()
-- If we don't have a texture to render, just draw a 100x100 placeholder box.
renderTexture Nothing  = renderPrimitive' GL.LineStrip red [ (0, 0)
                                                           , (fst noTexDims, 0)
                                                           , noTexDims
                                                           , (0, snd noTexDims)
                                                           , (0, 0)
                                                           ]
renderTexture (Just tex) = do GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
                              GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
                              GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)
                              -- Do nearest neighbor interpolation.

                              -- Enable texturing.
                              GL.texture GL.Texture2D $= GL.Enabled
                              GL.textureFunction      $= GL.Combine

                              -- Set current texture.
                              GL.textureBinding GL.Texture2D $= Just (texHandle tex)

                              -- Blam! Draw that textured square. We must move clockwise
                              -- from the top left of the image, so sayeth OpenGL.
                              GL.renderPrimitive GL.Polygon $ do texCoord' 0 0; vertex' left top;
                                                                 texCoord' 1 0; vertex' right top;
                                                                 texCoord' 1 1; vertex' right bottom;
                                                                 texCoord' 0 1; vertex' left bottom;

                              GL.texture GL.Texture2D $= GL.Disabled
    where
        left = 0
        right = texWidth tex
        top = texHeight tex
        bottom = 0

renderText :: String -> String -> IO ()
renderText _ _ = undefined

-- | Loads a texture, and returns a new renderer for it. NOTE: This function is
--   not thread-safe with respect to the texture cache.
--
--   Usage:
--
--   > coinTex <- textureRenderer "coin.png"
--   > updateWindow w $ coinTex { vAlign = VCenterAlign 0
--   >                          , hAlign = HCenterAlign 0
--   >                          , children = getCoinChildren
--   >                          }
textureRenderer :: TextureCache -> String -> IO Renderer
textureRenderer tc name = do tex <- loadTexture tc name
                             return $ defaultRenderer { render = renderTexture tex
                                                      , dims   = dims' tex
                                                      }
    where
        dims' tex = case tex of
                        Just tex' -> (texWidth tex', texHeight tex')
                        Nothing   -> noTexDims

textRenderer :: String -- ^ The name of the font we will use for rendering.
             -> String -- ^ The string we're drawing.
             -> Renderer
textRenderer fontName label = defaultRenderer { render = renderText fontName label
                                              }

-- | Renders any arbitrary OpenGL primitive.
renderPrimitive' :: GL.Color a
                 => PrimitiveMode -- ^ Which primitive we'll be drawing.
                 -> a             -- ^ The color of the primitive.
                 -> [(Int, Int)]  -- ^ A list of the primitive's verticies, in
                                 --   order (preferably clockwise). The tuple is
                                 --   (x, y).
                 -> IO ()
renderPrimitive' primTy col verts = do GL.color col
                                       GL.renderPrimitive primTy $ mapM_ (uncurry vertex') verts

-- | A simple demo renderer for a rectangle.
rectangleRenderer :: GL.Color a
                  =>  Int -- ^ The width of the desired rectangle.
                  -> Int  -- ^ The height of the desired rectangle.
                  -> a    -- ^ The color of the desired rectangle.
                  -> Renderer
rectangleRenderer width height col =
    defaultRenderer { render = renderPrimitive' GL.Polygon col [ (left, top)
                                                               , (right, top)
                                                               , (right, bottom)
                                                               , (left, bottom)
                                                               ]
                    , dims = (width, height)
                    }
    where
        top = height
        bottom = 0
        left = 0
        right = width
