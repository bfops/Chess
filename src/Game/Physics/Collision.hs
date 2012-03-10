{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
-- | Defines intersection tests for a varity of geometry.
module Game.Physics.Collision ( Point
                              , point2D 
                              , AABB
                              , aabb2D
                              , OBB
                              , obb2DCorners
                              , Intersectable(..)
                              ) where

import Data.List
import Numeric.LinearAlgebra

-- | "Axis-aligned bounding box."
--
--   Use the smart constructor 'aabb2D' to build AABBs.
--
--   * Implementation details:
--
--   An AABB is an Nx2 matrix. X[0] is the lower limit on the x-th dimension
--   and X[1] is the upper limit on the x-th dimension.
--
--   The number of rows is the number of dimensions of the box. The number of
--   columns is always 2. This is enforced by the smart constructor(s).
newtype AABB = AABB (Matrix Double)

-- | A point in the physics universe is just a list of the object's
--   coordinates. The length of the vector is the number of dimensions its's
--   defined for.
--
--   Use the smart constructor 'point2D' to build points.
newtype Point = Point (Vector Double)

-- | "Oriented Bounding Box." A bounding box with any arbitrary orientation.
--
--   Use smart constructor 'obb2D' to build OBBs.
--
--   * Implementation Notes:
--
--   An oriented bounding box consists of a rotation/translation matrix,
--   and a vector of n-dimensional extents. For example, a 2x2 bounding box
--   would have the extents [1,1].
--
--   We take a simplified approach in 2D - we just use the four corners of the
--   box, specified in a clockwise orientation. This turns our point-OBB
--   intersection test into 8 vector subtractions, and 4 dot products. Needles
--   to say, that's ridiculously cheap.
data OBB = OBB2D {-# UNPACK #-} !Point
                 {-# UNPACK #-} !Point
                 {-# UNPACK #-} !Point
                 {-# UNPACK #-} !Point
         | OBB !(Matrix Double) -- The transformation matrix.
               !(Vector Double) -- The extent matrix.

-- | Builds a 2D point out of a tuple.
point2D :: Integral a => (a, a) -> Point
point2D (x, y) = Point . fromList $ map fromIntegral [x, y]

-- | Constructs a 2-dimensional axis-aligned bounding box.
aabb2D :: (Double, Double) -> (Double, Double) -> AABB
aabb2D (xa, xb) (ya, yb) = AABB $ (2 >< 2) [ min xa xb, max xa xb
                                           , min ya yb, max ya yb
                                           ]

-- | Constructs a 2D oriented bounding box from a list of verticies, specified
--   in a clockwise direction.
--
--   Seriously - clockwise. Don't fuck this up. It's _very_ important. No
--   checks are done.
--
--   Clockwise.
obb2DCorners :: Point -> Point -> Point -> Point -> OBB
obb2DCorners = OBB2D

-- | The type class for our 'intersects' method. This lets us perform double
--   dispatch on the types of both of intersects' arguments.
class Intersectable a b where
    -- | We define intersects for as many pairs of geometry as possibly,
    --   allowing us to do double-dispatch collision detection.
    intersects :: a -> b -> Bool

between :: Ord a => (a, a) -> a -> Bool
between (l, r) x = l < x && x < r
{-# INLINE between #-}

instance Intersectable AABB AABB where
    -- | Two AABBs intersect if any of their edges lie between the other's
    --   edges.
    intersects (AABB ba) (AABB bb) = any testDim . zip (toRows ba) $ toRows bb
        where
            testDim :: (Vector Double, Vector Double) -> Bool
            testDim (a, b) = let a'@(lx, hx) = (a @> 0, a @> 1)
                                 b'@(ly, hy) = (b @> 0, b @> 1)
                              in  between a' ly || between a' hy
                               || between b' lx || between b' hx
    {-# INLINE intersects #-}

instance Intersectable AABB Point where
    intersects (AABB bs) (Point p) = all inRange . zip (toRows bs) $ toList p
        where
            inRange :: (Vector Double, Double) -> Bool
            inRange (bnds, x) = between (bnds @> 0, bnds @> 1) x
    {-# INLINE intersects #-}

instance Intersectable Point AABB where
    intersects x y = intersects y x
    {-# INLINE intersects #-}

obbBadInput :: a
obbBadInput = error "Invalid dimensions in the OOB intersection vectors being tested."

instance Intersectable OBB Point where
    intersects (OBB2D (Point w) (Point x) (Point y) (Point z)) (Point p)
                   | any (/= 2) [ dp, dw, dx, dy, dz ] = obbBadInput
                   | otherwise = all (>= 0) [ (p - w) <.> (x - w)
                                           , (p - x) <.> (y - x)
                                           , (p - y) <.> (z - y)
                                           , (p - z) <.> (w - z)
                                           ]
        where
            dp = dim p
            dw = dim w
            dx = dim x
            dy = dim y
            dz = dim z
    intersects (OBB lm exts) (Point p) | allEqual [ dlmx, dlmy, dp ] = obbBadInput
                                       | otherwise = intersects aabb transformed
        where
            transformed :: Point
            transformed = Point $ inv lm <> p
            aabb :: AABB
            aabb = AABB . fromRows . map (\e -> fromList [-e, e]) $ toList exts

            (dlmx, dlmy) = (cols lm, rows lm)
            dp           = dim p

-- | Are all elements in the list equal?
allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = fst $ foldl' (\(b, v) v' -> (b && v == v', v)) (True, x) xs
-- we use foldl', since in most cases, the elements _will_ be equal. If the
-- converse were true, I would prefer foldr since && is lazy.

instance Intersectable Point OBB where
    intersects x y = intersects y x
    {-# INLINE intersects #-}
