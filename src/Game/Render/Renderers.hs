{-# LANGUAGE RankNTypes #-}
module Game.Render.Renderers ( rectangleRenderer
                             , textureRenderer
                             , textRenderer
                             ) where

import Graphics.Rendering.OpenGL.Monad as GL
import Game.Engine
import Game.Texture
import Game.Render.Colors
import Game.Render.Core
import Util.HashString ( HashString )

renderText :: String -> String -> GL ()
renderText _ _ = undefined

-- | Given a loader, texture name, and renderer, returns a renderer drawing
--   the requested texture.
--
--   Usage:
--
--   > coinTex <- textureRenderer loader "coin.png"
--   > updateWindow w $ coinTex { vAlign = VCenterAlign 0
--   >                          , hAlign = HCenterAlign 0
--   >                          , children = getCoinChildren
--   >                          }
textureRenderer :: Loaders
                -> HashString -- ^ The name of the texture. Use the 'hashed' quasiquoter.
                -> Renderer
textureRenderer l n = case tex of
                        Just t -> defaultRenderer { render = renderTexture t
                                                 , rendDims = (texWidth t, texHeight t)
                                                 }
                        Nothing -> rectangleRenderer 10 10 red
    where
        tex = getResource (textureL l) n

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
                 -> GL ()
renderPrimitive' primTy col verts = do GL.color col
                                       GL.renderPrimitive primTy $ mapM_ (uncurry vertex') verts

-- | A simple debug renderer. Draw this if you're just experimenting with
--   layout.
rectangleRenderer :: GL.Color a
                  => Int  -- ^ The width of the desired rectangle.
                  -> Int  -- ^ The height of the desired rectangle.
                  -> a    -- ^ The color of the desired rectangle.
                  -> Renderer
rectangleRenderer width height col =
    defaultRenderer { render = renderPrimitive' GL.LineLoop
                                                col
                                                [ (left, top)
                                                , (right, top)
                                                , (right, bottom)
                                                , (left, bottom)
                                                ]
                    , rendDims = (width, height)
                    }
    where
        top = height
        bottom = 0
        left = 0
        right = width
