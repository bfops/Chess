-- | This module specifies all the colors we'll be using in-game. Never write
--   out a color manually in-game. Instead, put it in here so we can keep them
--   all consistent. Nothing's worse than having two slightly different shades
--   of green that need consolidating.
--
--   Okay, the holocaust. The holocaust was worse.
module UI.Colors where
-- No explicit export list. Just export everything.

import Graphics.Rendering.OpenGL.GL as GL

red, green, blue, white, black :: Color3 GLdouble

red = Color3 1.0 0.0 0.0
green = Color3 0.0 1.0 0.0
blue = Color3 0.0 0.0 1.0
white = Color3 1.0 1.0 1.0
black = Color3 0.0 0.0 0.0

-- | Used to tack an alpha value onto a normal color. Usage:
--
--   > red `withAlpha` 0.5 -- A half-transparent red.
--   > green `withAlpha` 1.0 -- Opaque green. In this case,
--   >                       -- withAlpha was unnecessary.
withAlpha :: Color3 a -> a -> Color4 a
withAlpha (Color3 r g b) = Color4 r g b

-- | Turns our double-specified color values into whatever stupid format OpenGL
--   thinks up next... assuming its an instance of Fractional.
clampify :: Fractional x => Color4 GLdouble -> Color4 x
clampify (Color4 a b c d) = Color4 (r2f a)
                                   (r2f b)
                                   (r2f c)
                                   (r2f d)
    where r2f = realToFrac
