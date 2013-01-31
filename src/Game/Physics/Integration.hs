{-# LANGUAGE BangPatterns #-}
module Game.Physics.Integration ( integrate
                                ) where

import Prelewd

import Control.Monad.ST
import Data.Packed.Development ( at' )
import Data.Packed.ST
import Data.Tuple
import Foreign.Storable ( Storable )
import qualified Data.Vector as D -- oh god this'll get confusing.
import Game.Physics.Types
import Numeric.GSL
import Numeric.LinearAlgebra hiding ( i )

-- | Converts a single physics object into its corresponding matrix
--   representation. This works for physics objects in any dimension.
--
--   > PObject (s_x s_y s_z)
--   >         (v_x v_y v_z)
--   > 
--   > [ s_x v_x ]T
--   > | s_y v_y |
--   > [ s_z v_z ]
asMatrix :: PhysicsObject -> Matrix Double
asMatrix p = fromRows [ s, v ]
    where
        s = pos p
        v = vel p

-- | Converts a list of physics objects into their corresponding combined
--   matrix by stacking their 'asMatrix' representations vertically.
bigMat :: [PhysicsObject] -> Matrix Double
bigMat = fromColumns . concatMap (toColumns . asMatrix)

-- | This function might be in Haskell, but it's just as dangerous as C.
memcpy :: Storable t
       => STVector s t -- ^ dest
       -> Int -- ^ destination offset
       -> Vector t -- ^ src
       -> Int -- ^ source offset
       -> Int -- ^ number of elements to copy
       -> ST s ()
memcpy dst dstoff src srcoff n = for 0 (<n) (+1) $
                                    \idx -> unsafeWriteVector dst (dstoff+idx) (src `at'` (srcoff+idx))
{-# INLINE memcpy #-}

-- | An emulation of a C for loop.
--
--   for (int i = first clause; second clause; third clause)
--       { loop body }
for :: Monad m
    => a -- First clause.
    -> (a -> Bool) -- Second clause.
    -> (a -> a) -- Third clause.
    -> (a -> m b) -- Loop body. Return value is ignored for convenience.
    -> m ()
for i0 condition inc body = go i0
    where
        go !i | condition i = body i >> go (inc i)
              | otherwise   = return ()
        {-# INLINE go #-}
{-# INLINE for #-}

integrate :: D.Vector PhysicsObject -> Double -> D.Vector PhysicsObject
integrate ps _ | D.length ps == 0 = ps
integrate ps t = D.map (\((PObject _ _ im f), [s', v']) -> PObject s' v' im f)
               . D.zip ps
               . D.map toRows
               . D.fromList -- vector fusion can't handle concatenation.
               . concat
               . toBlocksEvery 2 dims
               . reshape width
               . (! 1) . toRows  -- second row.
               . odeSolveV RKf45  -- Is there a better choice of integrator?
                           t      -- Not really sure about this. odeSolve uses t/100.
                           1.0e-5 -- arbitrary. Lower it if simulation blows
                                  -- up due to inaccuracy. Highten it for
                                  -- inceased performance.
                           1.0e-5 -- same as above.
                           integrator
                           Nothing -- TODO: Can we supply a jacobian one day?
                                   --       Is it recommended, or totally unnecessary?
                                   --       I have no idea. What the fuck is a jacobian?
                           initialConditions
                           $ fromList [0.0, t]
    where
        initialConditions :: Vector Double
        initialConditions = flatten . bigMat $ D.toList ps

        -- integrates by shifting all values over one to the left, and querying
        -- all the force functions for new accelerations. This is written
        -- imperatively since it rapes the GC if done functionally, and takes
        -- 2x as long as a result.
        --
        -- msrc matrix layout (after a (reshape width))
        --
        -- [ s_x_0 s_y_0 s_z_0   s_x_1 s_y_1 s_z_1  ... ]
        -- [ v_x_0 v_y_0 v_z_0   v_x_1 v_y_1 v_z_1  ... ]
        --
        integrator :: Double -> Vector Double -> Vector Double
        integrator dt vsrc = runSTVector $ do
                vdst <- newUndefinedVector (dim vsrc)
                memcpy vdst width vsrc 0 width

                let xs = (subVector 0     width vsrc)
                    vs = (subVector width width vsrc)
                    objectAt i = let j = i*dims
                                     p = ps D.! i
                                  in PObject (subVector j dims xs)
                                             (subVector j dims vs)
                                             (invMass p)
                                             (netForce p)

                for (0, 0) ((< len) . fst) ((+1) *** (+dims)) $ \(i, j) ->
                    let p  = ps D.! i
                        im = invMass p
                        f  = netForce p objectAt dt
                     in for 0 (<dims) (+1) $ \k ->
                            unsafeWriteVector vdst (j+k) (im * (f `at'` k))

                return vdst

        -- dimensions of each point/vector.
        dims = dim . pos $ ps D.! 0

        -- the number of particles we're integrating.
        len = D.length ps

        -- width of every row in the matrix.
        width = dims*len
