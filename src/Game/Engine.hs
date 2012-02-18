-- | In this module, we provide a basic framework for running a game.
module Game.Engine ( runGame
                   , Event(..)
                   , Dimensions
                   ) where

import           Config

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.DeepSeq

import qualified Data.Foldable as F
import           Data.Function
import           Data.Ratio
import           Data.Sequence ( Seq, (|>) )
import qualified Data.Sequence as Seq
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           Graphics.Rendering.OpenGL.Monad
import           Graphics.UI.GLUT ( Window
                                  , createWindow
                                  , Key(..)
                                  , KeyState(..)
                                  , Modifiers(..)
                                  , Position(..)
                                  , mainLoop
                                  )

import System.Log.Logger

import Util.Defs

data Event = KeyUp
           | KeyDown

instance NFData Event where
    rnf e = e `seq` ()

data GameState s = GameState { userState :: s
                             , windowDims :: Dimensions
                             }

-- | Starts the main loop of a game. Games will be run full screen and at the
--   maximum possible resolution.
runGame :: String
        -- ^ The title of the game window.
        -> gameState
        -- ^ The initial game's state.
        -> (gameState -> Dimensions -> GL ())
        -- ^ The 'render' function. Turns the state into something draw-able.
        --   This should do the minimum amount of work possible. All the hard
        --   stuff should be handled in the update function. The 'Dimensions'
        --   parameter is the current resolution of the display. Respect it!
        -> (gameState -> Double -> IO gameState)
        -- ^ Updates the game's state, given a current time (in seconds).
        --   All computationally expensive tasks should be done in here.
        -> (gameState -> Event -> gameState)
        -- ^ Updates the game's state given some input by the user.
        -> IO ()
runGame title initState rend updateT updateE =
    do runGraphics $ getArgsAndInitialize
                   >> initialDisplayMode $= [ DoubleBuffered
                                           , RGBAMode
                                           , WithSamplesPerPixel 2
                                           ]
       w <- createWindow title
       dims@(width, height) <- runGraphics $ initWindow w

       state <- atomically . newTVar $ GameState { userState = initState
                                                , windowDims = dims
                                                }

       eventQ <- atomically $ newTVar Seq.empty

       runGraphics $ do displayCallback       $= display state rend
                        reshapeCallback       $= Just (reshape state)
                        keyboardMouseCallback $= Just (onKeyMouse eventQ)
                        motionCallback        $= Just (onMotion eventQ)

       tid <- forkIO $ runUpdateLoop state updateT updateE eventQ

       mainLoop
       killThread tid

runUpdateLoop :: TVar (GameState a)
              -> (a -> Double -> IO a)
              -> (a -> Event -> a)
              -> TVar (Seq Event)
              -> IO ()
runUpdateLoop gs updateT updateE eventQ = getPOSIXTime >>= go
    where
        go :: NominalDiffTime -> IO ()
        go t1 = do t2 <- waitFor (t1 + framePeriod)
                   
                   (GameState us _) <- atomically $ readTVar gs

                   -- Update! We let the render thread read the old state while
                   -- we're updating, and clobber it afterwards. Although we
                   -- don't run it in the STM monad, the state is guaranteed
                   -- to be written to while we update.
                   us'  <- applyEventQueue us eventQ updateE
                   us'' <- updateT us' $ realToFrac t2
                   atomically $ do (GameState _ dims) <- readTVar gs
                                   writeTVar gs $ GameState us'' dims

                   go t2

-- | Waits for it to be at least the given time, and returns the time for "now"
--   as a convenience.
waitFor :: NominalDiffTime -> IO NominalDiffTime
waitFor targetTime = do currentTime <- getPOSIXTime
                        case compare currentTime targetTime of
                            EQ -> return currentTime
                            LT -> do threadDelay $ floor ((targetTime - currentTime) * 1e6)
                                     getPOSIXTime
                            GT -> do debugM "Game.Engine.update"
                                       $ "Missed the framerate deadline by " ++ show ((ceiling $ (currentTime - targetTime)*1e6) :: Integer) ++ " μs."
                                     return currentTime

-- | How long a frame is, in seconds.
framePeriod :: NominalDiffTime
framePeriod = realToFrac $ 1 % targetFramerate

-- | Clears the event queue, folding the event function over the current game
--   state. Returns the new userstate.
--
--   TODO: Verify this doesn't lock the game up if someone holds a key down.
applyEventQueue :: a
                -> TVar (Seq Event)
                -> (a -> Event -> a)
                -> IO a
applyEventQueue gs eventQ updateE = do
    q <- atomically $ do q <- readTVar eventQ
                         writeTVar eventQ Seq.empty
                         return q

    if Seq.null q
       then return gs
       else return $ F.foldl updateE gs q

-- | The GLUT 'displayCallback' hook.
display :: TVar (GameState a) -> (a -> Dimensions -> GL ()) -> IO ()
display gs rend = do (GameState u dimensions) <- atomically $ readTVar gs
                     runGraphics $ rend u dimensions

-- | The GLUT 'reshapeCallback' hook.
reshape :: TVar (GameState a) -> Size -> IO ()
reshape gs sz@(Size x y) = do atomically $ (writeTVar gs . update) =<< readTVar gs
                              runGraphics $ do viewport $= (Position 0 0, sz)
                                               matrixMode $= Projection
                                               loadIdentity
                                               ortho2D 0 (fromIntegral x) 0 (fromIntegral y)
                              infoM "Game.Engine.reshape" $ "Window dimensions: " ++ show x ++ "x" ++ show y
    where
        update gs' = gs' { windowDims = (fromIntegral x, fromIntegral y) }

-- | Adds an event onto the given event queue.
--   Make sure the event is fully evaluated before calling this function. This
--   saves us from having to do it later, or worse - in an STM transaction.
addEvent :: TVar (Seq Event) -> Event -> STM ()
addEvent q e = do q' <- readTVar q
                  writeTVar q $ q' |> e

-- TODO: Real events! This is pathetic.

onKeyMouse :: TVar (Seq.Seq Event)
           -> Key -> KeyState -> Modifiers -> Position -> IO ()
onKeyMouse updateE key keyState mods pos = return ()

onMotion :: TVar (Seq.Seq Event)
         -> Position -> IO ()
onMotion updateE pos = return ()

-- | Initializes a new window, and returns its dimensions.
initWindow :: Window -> GL Dimensions
initWindow w = do currentWindow $= Just w

                  windowSize $= uncurry (Size `on` fromIntegral) windowDimensions

                  -- Enable antialiasing, and general graphical nicities.
                  lineSmooth $= Enabled
                  pointSmooth $= Enabled
                  polygonSmooth $= Enabled
                  blend $= Enabled
                  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
                  lineWidth $= 1

                  mapM_ (\ty -> hint ty $= Nicest) [ PointSmooth
                                                  , LineSmooth
                                                  , PolygonSmooth
                                                  ]

                  return windowDimensions
