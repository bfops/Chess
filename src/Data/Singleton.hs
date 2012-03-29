module Data.Singleton where

import Data.Maybe

-- | A type is a singleton if it may contain zero or one of a specific value.
class Singleton f where
    -- | Extract value from a singleton
    single :: b
           -- ^ "Default" value to return if the singleton is devoid of the value we want.
           -> (a -> b)
           -- ^ If the singleton has a value to give us, pass it to this function.
           -> f a
           -- ^ Singleton to extract
           -> b

-- | Lists are singletons on their heads.
instance Singleton [] where
    single b f = single b f . listToMaybe

-- | Maybes are singletons on their Just values
instance Singleton Maybe where
    single = maybe

-- | Eithers are singletons on their Right values.
instance Singleton (Either a) where
    single = either . const
