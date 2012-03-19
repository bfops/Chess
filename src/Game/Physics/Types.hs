module Game.Physics.Types ( Position
                          , Velocity
                          , Force
                          , PhysicsObject(..)
                          ) where

import Control.DeepSeq
import Numeric.LinearAlgebra

type Position     = Vector Double
type Velocity     = Vector Double
type Force        = Vector Double

-- | Represents a simulatable object's current state. Pass a Data.Vector to of
--   these to the integrator to step the simulation.
data PhysicsObject = PObject { pos      :: Position
                             , vel      :: Velocity
                             , invMass  :: Double -- ^ 1/mass
                             -- | Given a function which retrieves a physics
                             --   object from its index in the vector and the
                             --   Δt from the start of the frame, return the
                             --   net force acting on the object.
                             --
                             --   The retrieved physics objects will have
                             --   updated positions and velocities.
                             , netForce :: (Int -> PhysicsObject)
                                        -> Double
                                        -> Force
                             }

instance Show PhysicsObject where
    show (PObject p v im _) = "pos = " ++ show p
                            ++ " | vel = " ++ show v
                            ++ " | invMass = " ++ show im

instance NFData PhysicsObject
