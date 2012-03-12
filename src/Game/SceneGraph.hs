{-# LANGUAGE BangPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
-- | A scene graph is used to represent any N-dimensional scene. Since it is
--   polymorphic over its contents, we can use it to represent a variety of
--   things, from a physics world to a render tree.
module Game.SceneGraph ( -- * Basic Data Types
                         SceneGraph(..)
                       , TransformView(..)
                       -- * Views
                       , viewTrans
                       , matRep
                       , Quaternion
                       -- * Transformations
                       , translate
                       , scaleGraph
                       , rotate2D
                       , rotate3D
                       , rotateQuat
                       , rotate3DX
                       , rotate3DY
                       , rotate3DZ
                       , rotateEuler
                       , applyPoint
                       , applyVector
                       -- * Graph Visitors
                       , optimize
                       , sceneMap
                       ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Par
import Data.Foldable ( Foldable )
import Data.Function.Pointless
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
    rnf (Transform t g) = rnf t `seq` rnf g
    rnf (Object x)      = rnf x
    rnf (Branch xs)     = rnf xs

-- | We represent the basic transformations (translate, scale, rotate) as their
--   "easy" representation, as well as the more complicated matrix. This allows
--   us to optimize the scene graph with the semantic information of chained
--   transformations instead of just blindly multiplying matricies.
--
--   For example, instead of multiplying two (rather complicated) quaternion
--   matricies to chain rotation, we just perform quaternion multiplication.
--
--   By keeping the matricies around too, we can always fall back on matrix
--   multiplication, and we can also avoid recomputing them.
--
--   Oh yeah, and all the matricies use homogenous coordinates, so they're
--   (N+1)x(N+1) in an n-dimensional scene. The more you know.
data Transformation = Translate !(Vector Double)
                                 (Matrix Double)
                    | Scale     !(Vector Double)
                                 (Matrix Double)
                    | Rotate2D  !Double -- θ - radians, clockwise.
                                 (Matrix Double) -- affine representation
                    | AffineTransform !(Matrix Double) -- fall back generic transformations
                                                       -- also used for rotations.
    deriving (Show, Typeable)

-- | A data type used for deconstructing transformations and seeing what they
--   "really are".
data TransformView = TranslateView !(Vector Double)
                   | ScaleView !(Vector Double)
                   | Rotate2DView !Double -- ^ θ - radians, clockwise.
                   | AffineTransformView !(Matrix Double) --  ^ Uses homogenous coordinates.

-- | Use this function to deconstruct a transformation into what it represents.
--
--   We use a more complicated internal data type for representing
--   transformations, and this view can be used to simplify it. The GHC extension
--   "ViewPatterns" will probably be of help here.
--
--   > -- f returns a scaled (by a factor of 2) translation vector.
--   > f :: Transformation -> Vector Double
--   > f (viewTrans -> Translate v) = v*2
viewTrans :: Transformation -> TransformView
viewTrans (Translate v _)     = TranslateView v
viewTrans (Scale v _)         = ScaleView v
viewTrans (Rotate2D θ _)      = Rotate2DView θ
viewTrans (AffineTransform m) = AffineTransformView m

-- | Retrieves the matrix view of a transformation. This will be an (N+1)x(N+1)
--   matrix, where N is the dimensions in the scene.
matRep :: Transformation -> Matrix Double
matRep (Translate _ m)     = m
matRep (Scale _ m)         = m
matRep (Rotate2D _ m)      = m
matRep (AffineTransform m) = m

seq_ :: a -> ()
seq_ = (`seq` ())

instance NFData Transformation where
    rnf (Translate _ m)     = seq_ m
    rnf (Scale     _ m)     = seq_ m
    rnf (Rotate2D  _ m)     = seq_ m
    rnf (AffineTransform _) = ()

-- | Just a 4-element vector, specifying a 3-dimensional rotation. See the
--   wikipedia page for more information on Quaternions.
type Quaternion = Vector Double

-- | Represents a 2-dimensional, clockwise rotation by θ radians. This is
--   probably easier to use than building a rotation matrix manually.
rotate2D :: Double -- ^ θ.
         -> Transformation
rotate2D !θ = Rotate2D θ $ rotate2DMat θ

rotate2DMat :: Double
            -> Matrix Double
rotate2DMat !θ = (3><3) [  c, s, 0
                        , -s, c, 0
                        , 0,  0, 1 ]
    where
        c = cos θ
        s = sin θ

-- | Rotates by a given quaternion.
--
--   Note: This algorithm is untested, and disagrees with wikipedia right now.
--         I'll look into it one day.
rotateQuat :: Quaternion -> Transformation
rotateQuat q = AffineTransform $ (4><4) [ 1-2.0*(yy-zz), 2.0*(xy-zw)    , 2.0*(xz+yw), 0
                                        , 2.0*(xy+zw)  , 1.0-2.0*(xx-zz), 2.0*(yz-xw), 0
                                        , 2.0*(xz-yw)  , 2*(xw+yz)      , 1-2*(xx-yy), 0
                                        , 0            , 0              , 0          , 1 ]
    where
        [ w, x, y, z ] = toList q
        xx = x*x
        yy = y*y
        zz = z*z
        zw = z*w
        xy = x*y
        xz = x*z
        xw = x*w
        yw = y*w
        yz = y*z

-- | Rotates by a given angle around an arbitrary 3-dimensional axis.
rotate3D :: Double -- ^ The amount (in radians) to rotate by.
         -> Vector Double -- ^ The 3-dimensional axis around which we are rotating.
         -> Transformation
-- https://en.wikipedia.org/wiki/Rotation_matrix#Rotation_matrix_from_axis_and_angle
rotate3D !θ !axis = AffineTransform $ (4><4) [ c+x*xoc , x*yoc-zs, x*zoc+ys, 0
                                             , y*xoc+zs, c+y*yoc , y*zoc-xs, 0
                                             , z*xoc-ys, z*yoc+xs, c+z*zoc , 0
                                             ,     0   ,    0    ,    0    , 1 ]
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
rotate3DX !θ = AffineTransform $ (4><4) [ 1,  0, 0, 0
                                        , 0,  c, s, 0
                                        , 0, -s, c, 0
                                        , 0,  0, 0, 1 ]
    where
        c = cos θ
        s = sin θ

-- | Rotates θ radians clockwise around the Y axis.
rotate3DY :: Double -- θ
          -> Transformation
rotate3DY !θ = AffineTransform $ (4><4) [  c, 0, -s, 0
                                        ,  0, 1,  0, 0
                                        , -s, 0,  c, 0
                                        ,  0, 0,  0, 1 ]
    where
        c = cos θ
        s = sin θ

-- | Rotates θ radians clockwise around the Y axis.
rotate3DZ :: Double -- θ
          -> Transformation
rotate3DZ !θ = AffineTransform $ (4><4) [  c,  s, 0, 0
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
rotateEuler x y z = let (AffineTransform x') = rotate3DX x
                        (AffineTransform y') = rotate3DY y
                        (AffineTransform z') = rotate3DZ z
                     in AffineTransform $ z' * y' * x'

-- | Adds a number to the end of a vector. Tends to be useful when constructing
--   homogenous coordinates.
vappend :: Element a => a -> Vector a -> Vector a
vappend x = join . flip (:) [ constant x 1 ]

-- | Applies the given modelview matrix to the given vector.
applyVector :: Matrix Double -> Vector Double -> Vector Double
applyVector m v = subVector 0 (dim v) $ m <> vappend 1.0 v

-- | Applied the given modelview matrix to the given point.
applyPoint :: Matrix Double -> Vector Double -> Vector Double
applyPoint m p = subVector 0 (dim p) $ m <> vappend 0.0 p

scaleMat :: Vector Double -> Matrix Double
scaleMat = diag . vappend 1.0

-- | Performs an n-dimensional scaling of the scene graph. Bigger numbers mean
--   bigger objects. 1.0 means no scaling.
scaleGraph :: Vector Double -- ^ The N-dimensional vector of values to scale by.
           -> Transformation
scaleGraph v = Scale v $ scaleMat v

translateMat :: Vector Double -> Matrix Double
translateMat v = fromColumns $ (init . toColumns . ident $ dim v + 1) ++ [vappend 1.0 v]

-- | Translates an n-dimensional scene graph by the given amount. Bigger values
--   mean bigger objects. 1.0 means no scaling.
translate :: Vector Double -- ^ The N-dimensional vector of values to translate by.
          -> Transformation
translate v = Translate v $ translateMat v

-- | Cleans up a scene graph, and does its best to avoid as many
--   multiplications and indirections as possible.
--
--   This will walk the whole graph, but if you're doing so anyhow, it'd be a
--   great way to avoid a lot of extra processing due to inefficient structure.
optimize :: SceneGraph a -> SceneGraph a
optimize x@(Object _) = x
optimize (Branch xs)  = optBranch xs
    where
        optBranch :: [SceneGraph a] -> SceneGraph a
        optBranch []  = Branch [] -- lolwut
        optBranch [x] = optimize x
        optBranch xs' = Branch $ foldr f [] xs'
            where
                -- merges sub-branches up into their parent.
                f y ys = case optimize y of
                            Branch zs -> zs ++ ys
                            g         -> g:ys
optimize (Transform t g) = go
    where
        go = let subGraph = optimize g
              in case subGraph of
                Transform t' subsubGraph -> case (t, t') of
                    (Translate v _, Translate v' _) -> Transform (combineTrans v v') subsubGraph
                    (Scale v _, Scale v' _)         -> Transform (combineScale v v') subsubGraph
                    (Rotate2D θ _, Rotate2D τ _)    -> Transform (combineRot θ τ) subsubGraph
                    (_, _)                          -> Transform t subGraph
                _ -> Transform t subGraph
        combineTrans = translate .: (+)
        combineScale = scaleGraph .: (+)
        combineRot   = rotate2D .: (+)
                
-- | Maps a function taking an object and its modelview matrix, returning a new
--   scene graph of a possibly different type.
--
--   Please optimize the scene graph before calling this function for optimal
--   efficiency.
--
--   To transform a point by the given matrix, use 'homPoint'. To
--   transform a vector, use 'homVector'.
--
--   Don't do the multiplication yourself. You'll probably get it wrong.
sceneMap :: NFData b
         => Int -- ^ The number of dimensions in the scene.
         -> (a -> Matrix Double -> b) -> SceneGraph a -> Par (SceneGraph b)
sceneMap n f = sMap (ident n)
    where
        -- Give a stack of matricies to multiply and a scene graph, returns the
        -- transformed (by f) scene graph.
        sMap m (Object x)  = return . Object $ f x m
        sMap m (Transform t child) = Transform t <$> sMap (matRep t * m) child
        sMap m (Branch xs) = Branch <$> parMapM (sMap m) xs
{-# INLINE sceneMap #-}
