{-# LANGUAGE BangPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
-- | A scene graph is used to represent any N-dimensional scene. Since it is
--   polymorphic over its contents, we can use it to represent a variety of
--   things, from a physics world to a render tree.
module Game.SceneGraph ( SceneGraph(..)
                       -- * Transformations
                       , translate
                       , scaleGraph
                       , rotate2D
                       , rotate3D
                       , rotate3DX
                       , rotate3DY
                       , rotate3DZ
                       , rotateEuler
                       , homPoint
                       , homVector
                       -- * Graph Visitors
                       , sceneMap
                       ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Par
import Data.Foldable ( Foldable )
import Data.Traversable
import Data.Typeable
import Numeric.LinearAlgebra

-- | Represents an N-dimensional scene graph, with objects at the leaves, and
--   transformations applied in a hierarchical fashion.
--
--   This is an instance of Functor, Foldable, and Traversable, so feel free to
--   walk it like you would any recursive data structure.
data SceneGraph a = Object a
                  | Transform !Transformation !(SceneGraph a)
                  | Branch [SceneGraph a]
    deriving (Show, Functor, Foldable, Traversable, Typeable)

instance NFData a => NFData (SceneGraph a) where
    rnf (Transform t g) = t `seq` rnf g
    rnf (Object x)      = rnf x
    rnf (Branch xs)     = rnf xs

-- | Any affine transformation can be represented by an (N+1)x(N+1) matrix,
--   where N is the dimensionality of the scene. We use the extra dimension to
--   put everything into homogenous coordinates. It just simplifies things.
type Transformation = Matrix Double

-- | Represents a 2-dimensional, clockwise rotation by θ radians. This is
--   probably easier to use than building a rotation matrix manually.
rotate2D :: Double -- ^ θ.
         -> Transformation
rotate2D !θ = (3><3) [  c, s, 0
                     , -s, c, 0
                     , 0,  0, 1 ]
    where
        c = cos θ
        s = sin θ

-- | Rotates by a given angle around an arbitrary 3-dimensional axis.
rotate3D :: Double -- ^ The amount (in radians) to rotate by.
         -> Vector Double -- ^ The 3-dimensional axis around which we are rotating.
         -> Transformation
-- https://en.wikipedia.org/wiki/Rotation_matrix#Rotation_matrix_from_axis_and_angle
rotate3D !θ !axis = (4><4) [ c+x*xoc , x*yoc-zs, x*zoc+ys, 0
                           , y*xoc+zs, c+y*yoc , y*zoc-xs, 0
                           , z*xoc-ys, z*yoc+xs, c+z*zoc , 0
                           ,     0   ,    0    ,    0    , 1
                           ]
    where
        c = cos θ
        oc = 1-c
        s = sin θ
        [x, y, z] = toList axis
        xs = x*s
        ys = y*s
        zs = z*s
        xoc = x*oc
        yoc = y*oc
        zoc = z*oc

-- | Rotates θ radians clockwise around the X-axis.
rotate3DX :: Double -- θ
          -> Transformation
rotate3DX !θ = (4><4) [ 1,  0, 0, 0
                      , 0,  c, s, 0
                      , 0, -s, c, 0
                      , 0,  0, 0, 1 ]
    where
        c = cos θ
        s = sin θ

-- | Rotates θ radians clockwise around the Y axis.
rotate3DY :: Double -- θ
          -> Transformation
rotate3DY !θ = (4><4) [  c, 0, -s, 0
                      ,  0, 1,  0, 0
                      , -s, 0,  c, 0
                      ,  0, 0,  0, 1 ]
    where
        c = cos θ
        s = sin θ

-- | Rotates θ radians clockwise around the Y axis.
rotate3DZ :: Double -- θ
          -> Transformation
rotate3DZ !θ = (4><4) [  c,  s, 0, 0
                      , -s,  c, 0, 0
                      ,  0,  0, 1, 0
                      ,  0,  0, 0, 1 ]
    where
        c = cos θ
        s = sin θ

-- | Rotates around a yaw, pitch, and roll - all in one transformation.
rotateEuler :: Double -- X-axis rotation.
            -> Double -- Y-axis rotation.
            -> Double -- Z-axis rotation.
            -> Transformation
rotateEuler x y z = rotate3DZ z * rotate3DY y * rotate3DX x

-- | Adds a number to the end of a vector. Tends to be useful when constructing
--   homogenous coordinates.
vappend :: Element a => a -> Vector a -> Vector a
vappend x = join . flip (:) [ constant x 1 ]

-- | Applies the given modelview matrix to the given vector, applying all the
--   stacked transformations in one go.
homVector :: Matrix Double -> Vector Double -> Vector Double
homVector m v = subVector 0 (dim v) $ m <> vappend 1.0 v

-- | Applied the given modelview matrix to the given point, applying all the
--   stacked transformations in one go.
homPoint :: Matrix Double -> Vector Double -> Vector Double
homPoint m p = subVector 0 (dim p) $ m <> vappend 0.0 p

-- | Performs an n-dimensional scaling of the scene graph. Bigger numbers mean
--   bigger objects. 1.0 means no scaling.
scaleGraph :: Vector Double -- ^ The N-dimensional vector of values to scale by.
           -> Transformation
scaleGraph = diag . vappend 1.0
{-# INLINE scaleGraph #-}

-- | Translates an n-dimensional scene graph by the given amount. Bigger values
--   mean bigger objects. 1.0 means no scaling.
translate :: Vector Double -- ^ The N-dimensional vector of values to translate by.
          -> Transformation
translate v = fromColumns $ (init . toColumns . ident $ dim v + 1) ++ [vappend 1.0 v]

-- | Maps a function taking an object and its modelview matrix, returning a new
--   scene graph of a possibly different type.
--
--   To transform a point by the given matrix, use 'homPoint'. To
--   transform a vector, use 'homVector'.
--
--   Don't do the multiplication yourself. You'll probably get it wrong.
sceneMap :: NFData b
         => Int -- ^ The number of dimensions in the scene.
         -> (a -> Matrix Double -> b) -> SceneGraph a -> SceneGraph b
sceneMap n f = runPar . sMap (ident n)
    where
        -- Give a stack of matricies to multiply and a scene graph, returns the
        -- transformed (by f) scene graph.
        sMap m (Object x)  = return . Object $ f x m
        sMap m (Transform t child) = Transform t <$> sMap (m * t) child
        sMap m (Branch xs) = Branch <$> parMapM (sMap m) xs
{-# INLINE sceneMap #-}
