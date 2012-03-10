{-# LANGUAGE BangPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
module Game.SceneGraph ( SceneGraph(..)
                       -- * Transformations
                       , translate
                       , scaleGraph
                       , rotate2D
                       -- * Graph Visitors
                       , optimize
                       , sceneMap
                       ) where

import Control.DeepSeq
import Data.Foldable ( Foldable )
import Data.Traversable
import Data.Typeable
import Numeric.LinearAlgebra

-- | Represents a 2D scene graph, with objects at the leaves, and
--   transformations applied in a hierarchical fashion.
--
--   This is an instance of Functor, Foldable, and Traversable, so feel free to
--   walk it like you would any recursive data structure.
data SceneGraph a = Object a
                  | Branch [SceneGraph a]
                  | Transform !Transformation !(SceneGraph a)
    deriving (Show, Functor, Foldable, Traversable, Typeable)

instance NFData a => NFData (SceneGraph a) where
    rnf (Transform t g) = t `seq` rnf g
    rnf (Branch xs)     = rnf xs
    rnf (Object x)      = rnf x

-- | Any affine transformation can be represented by an NxN matrix, where N
--   is the dimensionality of the scene.
type Transformation = Matrix Double

-- | Represents a 2-dimensional, clockwise rotation by θ radians. This is
--   probably easier to use than building a rotation matrix manually.
rotate2D :: Double -- ^ θ.
         -> Transformation
rotate2D !x = (2><2) [  c, s
                     , -s, c ]
    where
        c = cos x
        s = sin x

-- TODO: 3D rotation.

-- | Performs an n-dimensional scaling of the scene graph. Bigger numbers mean
--   bigger objects. 1.0 means no scaling.
scaleGraph :: Vector Double -- ^ The vector of values to scale by.
           -> Transformation
scaleGraph = diag
{-# INLINE scaleGraph #-}

-- | Translates an n-dimensional scene graph by the given amount. Bigger values
--   mean bigger objects. 1.0 means no scaling.
translate :: Vector Double -- ^ The vector of values to translate by.
          -> Transformation
translate v = fromColumns $ (init . toColumns . ident $ dim v) ++ [v]

-- | Maps a function taking an object and its modelview matrix, returning a new
--   scene graph of a possibly different type.
sceneMap :: Int -- ^ The number of dimensions in the scene.
         -> (a -> Matrix Double -> b) -> SceneGraph a -> SceneGraph b
sceneMap n f = sMap (ident n)
    where
        sMap m (Object x)  = Object $ f x m
        sMap m (Branch xs) = Branch $ map (sMap m) xs
        sMap m (Transform t child) = Transform t $ sMap (m <> t) child
{-# INLINE sceneMap #-}

-- | Optimizes the scene graph, combining as many nodes as possible.
--   You should probably pass your graph through this function as soon as its
--   fully built. This prevents duplicated computations from being carried out
--   by various engine submodules, and frees up some wasted memory and
--   indirection. All in all, it's a good thing. But it walks the full graph, so
--   you should probably only use this once per frame.
--
--   The returned scene graph will contain no thunks.
optimize :: NFData a => SceneGraph a -> SceneGraph a
optimize = force . optimize'
    where
        optimize' o@(Object _) = o
        -- multiply chained transformations into one.
        optimize' (Transform t g) = case g of
                                    (Transform t' g') -> Transform (t <> t') $ optimize g'
                                    _                 -> Transform t $ optimize g
        -- merge all child branches up into their parent.
        optimize' (Branch xs)  = Branch $ foldr f [] xs
            where
                f (Branch ys) xs' = map optimize ys ++ xs'
                f y           xs' = (optimize y):xs'
{-# INLINE optimize #-}
