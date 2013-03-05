{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, CPP, TupleSections #-}
-- | Defines the input engine for a game. 'InputState' is passed to every
--   call of a game's update function, denoting the current state of every key,
--   the mouse, and which mouse buttons are pressed.
module Game.Input ( -- * Input State Management.
                    InputState( mousePos )
                  , Key(..)
                  , KeyMask
                  , empty
                  , testKeys
                  -- * GLUT Hooks.
                  , onKeyMouse
                  , onMotion
                  ) where

import Prelewd hiding (empty)

import IO

import Impure

import Control.Concurrent.STM
import Control.DeepSeq
import Data.Bits
import Data.Char
import Data.List (partition)
import Data.Typeable
import Data.Tuple
import Text.Read
import Text.Show

import qualified Graphics.UI.GLUT as GLUT

import Util.Defs

-- We implement the input state as an integer, which is really a bitvector
-- representing whether a key is depressed or not.

-- | An input state is defined as a vector of whether a keyboard key or mouse
--   button is depressed, and the current mouse position.
data InputState = InputState { keys     :: KeyMask
                             , mousePos :: Coord
                             }

instance NFData InputState where
    rnf x = rnf (keys x)     `seq`
            rnf (mousePos x) `seq`
            ()

-- | A 'KeyState' represents a bitvector of whether or not certain keys have
--   been pressed.
newtype KeyMask = KM Integer
    deriving (Enum, Eq, Integral, Num, Ord, Bits, Read, Real, Show, NFData)

-- | An enum defining every key on the keyboard which may be pressed.
data Key = KeyChar Char -- ^ A keyboard character.
         | KeyF1
         | KeyF2
         | KeyF3
         | KeyF4
         | KeyF5
         | KeyF6
         | KeyF7
         | KeyF8
         | KeyF9
         | KeyF10
         | KeyF11
         | KeyF12
         | KeyLeft
         | KeyUp
         | KeyRight
         | KeyDown
         | KeyPageUp
         | KeyPageDown
         | KeyHome
         | KeyEnd
         | KeyInsert
         | KeyDelete 
         | KeyShift
         | KeyCtrl
         | KeyAlt
         | LeftButton   -- ^ Mouse button.
         | MiddleButton -- ^ Mouse button.
         | RightButton  -- ^ Mouse button.
         | WheelUp
         | WheelDown
    deriving (Ord, Eq, Typeable, Show)

instance NFData Key where
    rnf (KeyChar c) = c `seq` ()
    rnf      k      = k `seq` ()

#define KEY(gk, k) gsk2k gk = Just k

-- | "GLUT.SpecialKey to Key". Converts a GLUT.SpecialKey to our locally
--   defined 'Key'.
gsk2k :: GLUT.SpecialKey -> Maybe Key
KEY(GLUT.KeyF1,       KeyF1)
KEY(GLUT.KeyF2,       KeyF2)
KEY(GLUT.KeyF3,       KeyF3)
KEY(GLUT.KeyF4,       KeyF4)
KEY(GLUT.KeyF5,       KeyF5)
KEY(GLUT.KeyF6,       KeyF6)
KEY(GLUT.KeyF7,       KeyF7)
KEY(GLUT.KeyF8,       KeyF8)
KEY(GLUT.KeyF9,       KeyF9)
KEY(GLUT.KeyF10,      KeyF10)
KEY(GLUT.KeyF11,      KeyF11)
KEY(GLUT.KeyF12,      KeyF12)
KEY(GLUT.KeyLeft,     KeyLeft)
KEY(GLUT.KeyUp,       KeyUp)
KEY(GLUT.KeyRight,    KeyRight)
KEY(GLUT.KeyDown,     KeyDown)
KEY(GLUT.KeyPageUp,   KeyPageUp)
KEY(GLUT.KeyPageDown, KeyPageDown)
KEY(GLUT.KeyHome,     KeyHome)
KEY(GLUT.KeyEnd,      KeyEnd)
KEY(GLUT.KeyInsert,   KeyInsert)
KEY(GLUT.KeyDelete,   KeyDelete)
gsk2k _ = Nothing

#undef KEY

-- | "GLUT.Key to Key". Converts a GLUT.Key to our locally defined 'Key'.
gk2k :: GLUT.Key -> Maybe Key
gk2k (GLUT.Char c) = Just $ KeyChar c
gk2k (GLUT.SpecialKey ks) = gsk2k ks
gk2k (GLUT.MouseButton mb) = case mb of
                                 GLUT.LeftButton   -> Just LeftButton
                                 GLUT.MiddleButton -> Just MiddleButton
                                 GLUT.RightButton  -> Just RightButton
                                 GLUT.WheelUp      -> Just WheelUp
                                 GLUT.WheelDown    -> Just WheelDown
                                 _                 -> Nothing

isDown :: GLUT.KeyState -> Bool
isDown GLUT.Up   = False
isDown GLUT.Down = True

-- | "GLUT.Modifiers to Key". Converts the shift-ctrl-alt modifiers to the
--   keys the represent.
gm2k :: GLUT.Modifiers -> [(Key, Bool)]
gm2k (GLUT.Modifiers s c a) = [ (KeyShift, isDown s)
                              , (KeyCtrl,  isDown c)
                              , (KeyAlt,   isDown a)
                              ]

#define K2I(ctor, n) keyToIndex ctor = n

-- TODO: Convert the fromEnum into a more compact representation. We _really_
--       don't need the full ASCII charset here.
keyToIndex :: Key -> Int
keyToIndex (KeyChar c) = fromEnum c -- 0 - 255
K2I(KeyF1,        256)
K2I(KeyF2,        257)
K2I(KeyF3,        258)
K2I(KeyF4,        259)
K2I(KeyF5,        260)
K2I(KeyF6,        261)
K2I(KeyF7,        262)
K2I(KeyF8,        263)
K2I(KeyF9,        264)
K2I(KeyF10,       265)
K2I(KeyF11,       266)
K2I(KeyF12,       267)
K2I(KeyLeft,      268)
K2I(KeyUp,        269)
K2I(KeyRight,     270)
K2I(KeyDown,      271)
K2I(KeyPageUp,    272)
K2I(KeyPageDown,  273)
K2I(KeyHome,      274)
K2I(KeyEnd,       275)
K2I(KeyInsert,    276)
K2I(KeyDelete,    277)
K2I(KeyShift,     278)
K2I(KeyCtrl,      279)
K2I(KeyAlt,       280)
K2I(LeftButton,   281)
K2I(MiddleButton, 282)
K2I(RightButton,  283)
K2I(WheelUp,      284)
K2I(WheelDown,    285)

#undef K2I

-- | Updates a key mask with the new positions of the given keys. If depressed,
--   make the second value of the tuple True, otherwise, if the key is up, make
--   it False.
updateKeyMask :: KeyMask -> [(Key, Bool)] -> KeyMask
updateKeyMask m ks = let (pushed, released) = (map fst *** map fst) . partition snd $ map2 keyToIndex <$> ks
                         released' = foldl' clearBit m         released
                      in             foldl' setBit   released' pushed

-- | No keys depressed, mouse in the bottom-left. This is used as a placeholder
--   before real updates are run.
--
--   Use this once, before any updates roll in. Afterwards, just 'clear' your
--   existing InputState to maintain things that don't change, such as mouse
--   position.
empty :: InputState
empty = InputState (KM 0) (0, 0)

-- | Tests if a group of keys are pressed in the given input state.
testKeys :: InputState -> [Key] -> Bool
testKeys is = all (testBit (keys is) . keyToIndex)
{-# INLINE testKeys #-}

-- | A GLUT 'keyboardMouseCallback' that should be partially applied with a
--   shared 'InputState' variable. This will keep the state updated with the
--   latest keyboard/mouse events.
--
--   We need access to a TVar with the window dimensions so that we can flip
--   axes accordingly. Fucking useless GLUT.
onKeyMouse :: TVar InputState
           -> TVar Dimensions
           -> GLUT.Key
           -> GLUT.KeyState
           -> GLUT.Modifiers
           -> GLUT.Position
           -> SystemIO ()
onKeyMouse tis dims k ks ms (GLUT.Position x y) = atomically $
    do InputState keyboard _ <- readTVar tis
       (_, dy)               <- readTVar dims
       let k' = gk2k k
           km' = gm2k ms
        in writeTVar tis $!! InputState
                               (updateKeyMask keyboard $ (k' <&> (,isDown ks) <&> (:[]) <?> []) <> km')
                               (fromIntegral x, dy-fromIntegral y) -- flip the y-coord. Fucking GLUT.

-- | A GLUT 'motionCallback' that should be partially applied with a shared
--   'InputState' variable. This will keep the state updated with the latest
--   mouse position.
--
--   We need access to a TVar with the window dimensions so that we can flip
--   axes accordingly. Fucking useless GLUT.
onMotion :: TVar InputState
         -> TVar Dimensions
         -> GLUT.Position
         -> SystemIO ()
onMotion tis dims (GLUT.Position x y) = atomically $
    do InputState k _ <- readTVar tis
       (_, dy)        <- readTVar dims
       writeTVar tis $!! InputState k (fromIntegral x, dy-fromIntegral y) -- flip the y-coord. Fucking GLUT.
