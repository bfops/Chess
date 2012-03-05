{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}
-- | Defines intersection tests for a varity of geometry.
module Game.Physics.Collision ( Point(..)
                              , AABB(..)
                              , OBB(..)
                              , Intersectable(..)
                              ) where

import Numeric.Container

-- | "Axis-aligned bounding box."
--
--   Holds the values of the edges. These two vectors must be of the same
--   length. highs[i] and lows[i] are the high and low box bounds in the ith
--   dimension.
data AABB = AABB { highs :: Vector Float
                 , lows  :: Vector Float
                 }

-- | A point in the physics universe is just a list of the object's
--   coordinates. The length of the vector is the number of dimensions its's
--   defined for.
newtype Point = Point (Vector Float)

-- | "Oriented bounding box." A bounding box with any arbitrary orientation.
--
--   The list is 4 elements long, and stores corners in a clockwise
--   direction. Please maintain these invariants until I find a way to do it
--   with the typechecker.
--
--   For the time being, this is only impelmented in 2 dimensions. You will
--   get a runtime error with points in any other dimension, or a 'corners'
--   list which is not 4 elements long.
data OBB = OBB { corners :: [Point]
               }

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
    intersects (AABB hxs lxs) (AABB hys lys) = any testDim . enumFromTo 0 . (subtract 1) $ minimum [ dim hxs, dim lxs, dim hys, dim lys ]
        where
            testDim :: Int -> Bool
            testDim i = let hx = hxs @> i
                            lx = lxs @> i
                            hy = hys @> i
                            ly = lys @> i
                         in  between (lx, hx) ly || between (lx, hx) hy
                          || between (ly, hy) lx || between (ly, hy) hx
    {-# INLINE intersects #-}

instance Intersectable AABB Point where
    intersects (AABB hs ls) (Point p) = all inRange . enumFromTo 0 . (subtract 1) $ minimum [dim hs, dim ls, dim p]
        where
            inRange :: Int -> Bool
            inRange i = between (ls @> i, hs @> i) $ p @> i
    {-# INLINE intersects #-}

instance Intersectable Point AABB where
    intersects x y = intersects y x
    {-# INLINE intersects #-}

obbBadInput :: a
obbBadInput = error "We don't define OBB-point intersections for any dimensions except for 2."

instance Intersectable OBB Point where
    intersects (OBB [Point w, Point x, Point y, Point z]) (Point p)
                   | dim p /= 2 = obbBadInput
                   | otherwise = all (>= 0) [ p `sub` w <.> x `sub` w
                                           , p `sub` x <.> y `sub` x
                                           , p `sub` y <.> z `sub` y
                                           , p `sub` z <.> w `sub` z
                                           ]
    intersects _ _ = obbBadInput
    {-# INLINE intersects #-}

instance Intersectable Point OBB where
    intersects x y = intersects y x
    {-# INLINE intersects #-}
