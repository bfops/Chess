-- | A utility module containing data definitions common to the rest of the
--   code. This file is forbidden from having dependencies on any other part
--   of the codebase, except for other libraries (such as
--   Graphics.Rendering.OpenGL.Monad and its cousins).
--
--   In all cases, the dependency graph should be acyclic.
module Util.Defs ( Coord
                 , Dimensions
                 , cyclicNext
                 , cyclicPrev
                 ) where

-- | The (x, y) coordinates of a point on the screen, measured from the bottom,
--   left corner.
type Coord = (Int, Int)

-- | If you were to draw an axis-aligned bounding box around an object,
--   Dimensions would represent the (x, y) lengths of the sides.
type Dimensions = (Int, Int)

cyclicNext :: (Enum a, Bounded a, Eq a) => a -> a
cyclicNext a = if a == maxBound
                then minBound
                else succ a

cyclicPrev :: (Enum a, Bounded a, Eq a) => a -> a
cyclicPrev a = if a == minBound
                then maxBound
                else pred a

