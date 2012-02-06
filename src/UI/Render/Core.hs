module UI.Render.Core ( Offset
                      , HAlign(..)
                      , VAlign(..)
                      , Position
                      , Dimensions
                      , Renderer(..)
                      , defaultRenderer
                      , updateWindow
                      ) where

import Control.Monad
import Graphics.UI.GLUT

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
data HAlign = LeftAlign    !Offset
            | RightAlign   !Offset
            | HCenterAlign !Offset

-- | A type used for specifying vertical alignment.
data VAlign = TopAlign     !Offset
            | BottomAlign  !Offset
            | VCenterAlign !Offset

-- | The (x, y) coordinates of a point on the screen, measured from the bottom,
--   left corner.
type Coord = (Int, Int)

-- | If you were to draw an axis-aligned bounding box around an object,
--   Dimensions would represent the (x, y) lengths of the sides.
type Dimensions = (Int, Int)

-- | A renderable is anything which can be drawn onto the screen.
--   To build one, use renderer and override the necessary arguments.
--
--   > rect = defaultRenderer { draw = drawRectangle
--   >                        , pos = (10, 4)
--   >                        , dims = (50, 50)
--   >                        }
data Renderer = Renderer { render :: IO () -- ^ Draws the object onto the screen. You
                                      --   don't have to worry about positioning,
                                      --   as this is automatically handled using
                                      --   the 'pos' field.

                         , pos :: Coord -- ^ The position of the bottom-left
                                       --   corner of the object, relative
                                       --   to the bottom left corner of the
                                       --   object's parent.

                         , dims :: Dimensions -- ^ The dimensions of the object.

                         -- | The coordinates of the point to rotate around,
                         --   relative to the bottom-left of the object.
                         , rotateAround :: Coord

                         -- | The amount to rotate the object by, in radians.
                         --   Positive values are clockwise.
                         , rotation :: Double

                         -- | An optional vertical alignment for the object,
                         --   relative to its parent. If this is set, the
                         --   y-component of the 'pos' field will be ignored.
                         , vAlign :: Maybe VAlign

                         -- | An optional horizontal alignment for the object,
                         --   relative to its parent. If this is set, the
                         --   x-component of the 'pos' field will be ignored.
                         , hAlign :: Maybe HAlign

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
--   >                        , dims = (50, 50)
--   >                        }
--   
--   See: 'Renderer'
defaultRenderer :: Renderer
defaultRenderer = Renderer { render = return ()
                           , pos = (0, 0)
                           , dims = (0, 0)
                           , rotateAround = (0, 0)
                           , rotation = 0.0
                           , vAlign = Nothing
                           , hAlign = Nothing
                           , children = []
                           }

-- | Returns the position of a renderer, accounting for any alignment that may
--   take place.
absPos :: Dimensions -- ^ The dimensions of the parent object.
       -> Renderer  -- ^ The renderer whose new position we need.
       -> Coord
absPos parentDims r = let x' = case hAlign r of
                                  Nothing                 -> x
                                  Just (LeftAlign off)    -> off
                                  Just (RightAlign off)   -> pdx - dx + off
                                  Just (HCenterAlign off) -> (pdx - dx) `div` 2 + off

                          y' = case vAlign r of
                                  Nothing                 -> y
                                  Just (TopAlign off)     -> off
                                  Just (BottomAlign off)  -> pdy - dy + off
                                  Just (VCenterAlign off) -> (pdy - dy) `div` 2 + off
                       in (x', y')
    where
        (x, y) = pos r
        (dx, dy) = dims r
        (pdx, pdy) = parentDims

rad2deg :: Double -> Double
rad2deg rad = rad * 180 / pi
{-# INLINE rad2deg #-}

-- | Draws the given renderer, and all its children to the provided window.
updateWindow :: Window -> Renderer -> IO ()
updateWindow w rs = do currentWindow $= Just w
                       Size x y <- get windowSize
                       clear [ ColorBuffer ]
                       loadIdentity
                       render' (fromIntegral x, fromIntegral y) rs
                       swapBuffers
    where
        render' parentDims r = preservingMatrix $ do
                               let (x, y) = absPos parentDims r
                               translate $ Vector3 (fromIntegral x) (fromIntegral y) (0 :: GLdouble)
                               applyRotation (rotation r) (rotateAround r)
                               render r
                               forM_ (children r) $ render' (dims r)

-- | To apply a rotation around a point, translate to that point, apply
--   the rotation, then translate backwards. To visualize this, picture
--   rotating a sphere in an orbit.
applyRotation :: Double -> (Int, Int) -> IO ()
applyRotation rads (xrot, yrot) = do translate $ Vector3 (fromIntegral xrot) (fromIntegral yrot) (0 :: GLdouble)
                                     rotate (realToFrac . rad2deg $ rads) $ Vector3 0 0 (-1.0 :: GLdouble)
                                     translate $ Vector3 (fromIntegral $ -xrot) (fromIntegral $ -yrot) (0 :: GLdouble)
