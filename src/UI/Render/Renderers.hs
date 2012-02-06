module UI.Render.Renderers ( textureRenderer
                           ) where

import Codec.Picture
--import Codec.Picture.Types -- needed for imageWidth / imageHeight
import UI.Render.Core

renderTexture :: DynamicImage -> IO ()
renderTexture _ = undefined

imageWidth' :: DynamicImage -> Int
imageWidth' _ = undefined

imageHeight' :: DynamicImage -> Int
imageHeight' = undefined

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
textureRenderer name = do Right rawTex <- readImage name
                          return $ defaultRenderer { render = renderTexture rawTex
                                                   , dims   = (imageWidth' rawTex, imageHeight' rawTex)
                                                   }
