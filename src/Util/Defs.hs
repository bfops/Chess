-- | A utility module containing data definitions common to the rest of the
--   code. This file is forbidden from having dependencies on any other part
--   of the codebase, except for other libraries (such as
--   Graphics.Rendering.OpenGL.Monad and its cousins).
--
--   In all cases, the dependency graph should be acyclic.
module Util.Defs
    where

-- | The (x, y) coordinates of a point on the screen, measured from the bottom,
--   left corner.
type Coord = (Int, Int)

-- | If you were to draw an axis-aligned bounding box around an object,
--   Dimensions would represent the (x, y) lengths of the sides.
type Dimensions = (Int, Int)

iff :: Bool -> a -> a -> a
iff b x y = if b then x else y
