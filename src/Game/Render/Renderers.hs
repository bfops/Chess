{-# LANGUAGE RankNTypes, TemplateHaskell #-}
-- | Contains helper functions for ezpz definition of a variety of primitive
--   renderers.
module Game.Render.Renderers ( rectangleRenderer
                             --, textureRenderer -- NEIN! Use makeTextureRenderer.
                             , texRend
                             , textRenderer
                             ) where

import Prelewd

import IO
import Impure

import qualified Config
import Data.HashString
import Data.String (String)
import qualified Data.Text as T
import Data.Tuple
import Graphics.Rendering.OpenGL.Monad as GL
import Game.Engine
import Game.Render.Colors
import Game.Render.Core
import Game.Resource.Texture
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.FilePath
import System.IO.Unsafe ( unsafePerformIO ) -- Only used in TH, to load dimensions.

import Util.Defs

renderText :: String -> String -> Loaders -> GL ()
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
textureRenderer :: HashString -- ^ The name of the texture. Use the 'hashed' quasiquoter.
                -> Dimensions
                -> Renderer
textureRenderer n ds = defaultRenderer { render = maybe failRender renderTexture . loadResource n
                                       , rendDims = ds
                                       }
    where
          failRender = rectRender ds red
          loadResource = flip $ getResource . textureL

getTexDims :: String -> SystemIO Dimensions
getTexDims s = maybe textureError (imageWidth' &&& imageHeight') <$> getImage (T.pack s)
    where textureError = error $ "Texture \"" <> s <> "\" not found at " <> Config.texturePrefix </> s

texRenderQuoter :: String -> Q Exp
texRenderQuoter s = [| textureRenderer $(hString s') $(lift . unsafePerformIO $ getTexDims s') |]
    where
        s' = filter (/= '"') s

-- | Usage : let dot = [texRend|"yellow-dot.png"|] :: Renderer
texRend :: QuasiQuoter
texRend = QuasiQuoter { quoteExp  = texRenderQuoter
                      , quotePat  = undefined
                      , quoteType = undefined
                      , quoteDec  = undefined
                      }

-- | Renders a string onto the screen (2D).
--
--   _This function is currently incomplete. Do not use it!_
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
                                       GL.renderPrimitive primTy $ traverse_ (uncurry vertex') verts

-- | A simple debug renderer. Draw this if you're just experimenting with
--   layout.
rectangleRenderer :: GL.Color a
                  => Int  -- ^ The width of the desired rectangle.
                  -> Int  -- ^ The height of the desired rectangle.
                  -> a    -- ^ The color of the desired rectangle.
                  -> Renderer
rectangleRenderer width height col =
    defaultRenderer { render = \_-> rectRender (width, height) col
                    , rendDims = (width, height)
                    }

rectRender :: GL.Color a => (Int, Int) -> a -> GL.GL ()
rectRender (width, height) col = renderPrimitive' GL.LineLoop
                                                  col
                                                  [ (left, top)
                                                  , (right, top)
                                                  , (right, bottom)
                                                  , (left, bottom)
                                                  ]
    where
        top = height
        bottom = 0
        left = 0
        right = width
