module Control.Combinator ( fmap1, fmap2, fmap3, fmap4
                          , ap1, ap2, ap3, ap4
                          , on1, on2, on3, on4
                          )
    where

import Control.Applicative

fmap0 :: (a -> b) -> a -> b
fmap1 :: Functor f => (a -> b) -> f a -> f b
fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap3 :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
fmap4 :: (Functor f1, Functor f2, Functor f3, Functor f4) => (a -> b) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))

fmap0 = id
fmap1 = fmap0.fmap
fmap2 = fmap1.fmap
fmap3 = fmap2.fmap
fmap4 = fmap3.fmap

infixl 3 `fmap0`
infixl 3 `fmap1`
infixl 3 `fmap2`
infixl 3 `fmap3`
infixl 3 `fmap4`

ap1 :: (a -> b) -> a -> b
ap2 :: (a1 -> a2 -> b) -> a2 -> a1 -> b
ap3 :: (a1 -> a2 -> a3 -> b) -> a3 -> a1 -> a2 -> b
ap4 :: (a1 -> a2 -> a3 -> a4 -> b) -> a4 -> a1 -> a2 -> a3 -> b

ap1 f x = ($x) `fmap0` f
ap2 f x = ($x) `fmap1` f
ap3 f x = ($x) `fmap2` f
ap4 f x = ($x) `fmap3` f

infixr 1 `ap1`
infixr 1 `ap2`
infixr 1 `ap3`
infixr 1 `ap4`

on1 :: (a -> b) -> (r -> a) -> (r -> b)
on2 :: (a -> b -> c) -> (r -> b) -> (a -> r -> c)
on3 :: (a -> b -> c -> d) -> (r -> c) -> (a -> b -> r -> d)
on4 :: (a -> b -> c -> d -> e) -> (r -> d) -> (a -> b -> c -> r -> e)

on1 f g = (<$> g) `fmap0` f
on2 f g = (<$> g) `fmap1` f
on3 f g = (<$> g) `fmap2` f
on4 f g = (<$> g) `fmap3` f

infixl 4 `on1`
infixl 4 `on2`
infixl 4 `on3`
infixl 4 `on4`

