-- | The render core defines the basic types and helper functions for a basic
--   UI system.
--
--   The fundamental data type is the 'Renderer', which is declared in a
--   hierarchical fashion.
module UI.Render.Core ( Renderer(..)
                      , Offset
                      , defaultRenderer
                      , updateWindow
                      , HAlign(..)
                      , VAlign(..)
                      ) where

import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL.Monad
import Util.Defs

-- | Used for specifying the nudge factors during alignment.
--   These values, along with anything else in this library,
--   are always using the following coordinate system:
--
--   @
--      +
--      ^
--      |
--      |
--      |
--      |
--      +---------> +
--    (0,0)
--   @
type Offset = Int

-- | A type used for specifying horizontal alignment.
--   each alignment aligns part side of the object with that part of the parent.
data HAlign = LeftAlign    !Offset
            | RightAlign   !Offset
            | HCenterAlign !Offset

-- | A type used for specifying vertical alignment.
--   each alignment aligns that part of the object with that part of the parent.
data VAlign = TopAlign     !Offset
            | BottomAlign  !Offset
            | VCenterAlign !Offset

-- | A renderable is anything which can be drawn onto the screen.
--   To build one, use renderer and override the necessary arguments.
--
--   > rect = defaultRenderer { draw = drawRectangle
--   >                        , pos = (10, 4)
--   >                        , rendDims = (50, 50)
--   >                        }
data Renderer = Renderer { render :: GL () -- ^ Draws the object onto the screen. You
                                           --   don't have to worry about positioning,
                                           --   as this is automatically handled using
                                           --   the 'pos' field.

                         , pos :: Either Coord -- | Specify absolute position
                                               --   of the bottom-left corner
                                               --   of the object.
                                         (HAlign, VAlign) -- | Specify alignment
                                                          --   relative to the
                                                          --   parent

                         , rendDims :: Dimensions -- ^ The dimensions of the object.

                         -- | The coordinates of the point to rotate around,
                         --   relative to the bottom-left of the object.
                         --   If Nothing, rotation is around the middle of the
                         --   object.
                         , rotateAround :: Maybe Coord

                         -- | The amount to rotate the object by, in radians.
                         --   Positive values are clockwise.
                         , rotation :: Double

                         -- | Any child renderers are listed here. When the UI
                         --   library renders a frame, first the current
                         --   'Renderer's 'render' method is called, then the
                         --   children are rendered on top.
                         , children :: [Renderer]
                         }

-- | The default renderer. Draws absolutely nothing. Overload its fields with:
--
--   > rect = defaultRenderer { draw = (drawRectangle 50 50)
--   >                        , pos = (10, 4)
--   >                        , rendDims = (50, 50)
--   >                        }
--
--   See: 'Renderer'
defaultRenderer :: Renderer
defaultRenderer = Renderer { render = return ()
                           , pos = Left (0, 0)
                           , rendDims = (0, 0)
                           , rotateAround = Nothing
                           , rotation = 0.0
                           , children = []
                           }

-- | Returns the position of a renderer, accounting for any alignment that may
--   take place.
absPos :: Dimensions -- ^ The dimensions of the parent object.
       -> Renderer  -- ^ The renderer whose new position we need.
       -> Coord
absPos parentDims r = case pos r of
                        Left p -> p
                        Right (hAlign, vAlign) -> let
                          x' = case hAlign of
                                  LeftAlign off    -> px + off
                                  RightAlign off   -> px - dx + off
                                  HCenterAlign off -> (px - dx) `div` 2 + off

                          y' = case vAlign of
                                  BottomAlign off  -> py + off
                                  TopAlign off     -> py - dy + off
                                  VCenterAlign off -> (py - dy) `div` 2 + off
                          in (x', y')
                          where (dx, dy) = rendDims r
                                (px, py) = parentDims

rad2deg :: Double -> Double
rad2deg rad = rad * 180 / pi
{-# INLINE rad2deg #-}

-- | Draws the given renderer, and all its children to the current window.
--   This should be called every frame, for every window on the screen.
--
--   Make sure to pass in the dimensions of the region on which we should draw.
--   Normally, this will the the `get windowSize` of the current window.
updateWindow :: Dimensions -> Renderer -> GL ()
updateWindow rootDims rs = do clear [ ColorBuffer ]
                              matrixMode $= Modelview 0
                              loadIdentity
                              render' rootDims rs
                              swapBuffers
    where
        render' parentDims r = preservingMatrix $ do
                               let (x, y) = absPos parentDims r
                                   (w, h) = rendDims r
                                   center = (x + w `div` 2, y + h `div` 2)
                               translate $ Vector3 (fromIntegral x) (fromIntegral y) (0 :: GLdouble)
                               applyRotation (rotation r) (fromMaybe center $ rotateAround r)
                               render r
                               forM_ (children r) $ render' (rendDims r)

-- | To apply a rotation around a point, translate to that point, apply
--   the rotation, then translate backwards. To visualize this, picture
--   rotating a sphere in an orbit.
applyRotation :: Double -> (Int, Int) -> GL ()
applyRotation rads (xrot, yrot) = do translate $ Vector3 (fromIntegral xrot) (fromIntegral yrot) (0 :: GLdouble)
                                     rotate (realToFrac . rad2deg $ rads) $ Vector3 0 0 (-1.0 :: GLdouble)
                                     translate $ Vector3 (fromIntegral $ -xrot) (fromIntegral $ -yrot) (0 :: GLdouble)
