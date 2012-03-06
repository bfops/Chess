{-# LANGUAGE BangPatterns #-}
-- | In this module, we provide a basic framework for running a game.
module Game.Engine ( runGame
                   , Loaders ( textureL )
                   , getResource
                   ) where

import           Config

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.Function
import           Data.IORef
import qualified Data.Text     as T
import           Data.Ratio
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import Game.Input as I
import Game.Loaders
import Game.Render.Core
import Game.Resource.Loader

import           Graphics.Rendering.OpenGL.Monad
import           Graphics.UI.GLUT ( Position(..)
                                  , mainLoop
                                  )

import System.Log.Logger

import Util.Defs

data GameState s = GameState { userState  :: IORef s -- only read/written in the update thread.
                             , toRender   :: TVar Renderer
                             , windowDims :: TVar Dimensions
                             , inputSt    :: TVar InputState
                             , loaders    :: MVar Loaders
                             }

-- | Starts the main loop of a game. Games will be run full screen and at the
--   maximum possible resolution.
runGame :: T.Text
        -- ^ The title of the game window.
        -> gameState
        -- ^ The initial game's state.
        -> (gameState ->
           Double ->
           InputState -> IO (gameState,
                            [ResourceRequest],
                            Renderer))
        -- ^ Updates the game's state, given a current time (in seconds), and
        --   the keyboard/mouse input vector. All computationally expensive
        --   tasks should be done in here.
        --
        --   Resources required for rendering the described scene must be
        --   specified in the second element of the returned tuple.
        --
        --   The third element in the returned value will be used for drawing
        --   the scene described by the update function.
        -> IO ()
runGame title initState updateT = do runGraphics $ getArgsAndInitialize
                                                 >> initialDisplayMode $= [ DoubleBuffered
                                                                         , RGBAMode
                                                                         , Multisampling
                                                                         , WithSamplesPerPixel 2
                                                                         ]

                                     w <- runGraphics $ initWindow title windowDimensions

                                     state <- GameState <$> newIORef   initState
                                                       <*> atomically (newTVar defaultRenderer)
                                                       <*> atomically (newTVar windowDimensions)
                                                       <*> atomically (newTVar I.empty)
                                                       <*> newMVar    (Loaders emptyResourceLoader)

                                     runGraphics $ do displayCallback       $= display state w
                                                      reshapeCallback       $= Just (reshape state)
                                                      keyboardMouseCallback $= Just (onKeyMouse (inputSt state) $ windowDims state)
                                                      motionCallback        $= Just (onMotion   (inputSt state) $ windowDims state)
                                                      passiveMotionCallback $= Just (onMotion   (inputSt state) $ windowDims state)

                                     tid <- forkIO $ runUpdateLoop state updateT

                                     mainLoop
                                     killThread tid

runUpdateLoop :: GameState a
              -> (a -> Double -> InputState -> IO (a, [ResourceRequest], Renderer))
              -> IO ()
runUpdateLoop gs updateT = getPOSIXTime >>= go
    where
        go :: NominalDiffTime -> IO ()
        go t1 = do t2 <- waitFor (t1 + framePeriod)
 
                   us <- readIORef $ userState gs
                   is <- atomically . readTVar $ inputSt gs

                   -- Update! We let the render thread use our last renderer
                   -- while we're updating, and we clobber it afterwards.
                   -- Although we don't run it in the STM monad, the
                   -- renderFunc is guaranteed to be read-only while we
                   -- update. If this is broken, fix the offending code!
                   (us', reqs, rend) <- updateT us (realToFrac t2) is

                   writeIORef (userState gs) us'

                   modifyMVar_ (loaders gs) $ \ls ->
                          do ls' <- updateLoaders ls reqs
                             atomically $ writeTVar (toRender gs) rend
                             return ls'

                   go t2

-- | Waits for it to be at least the given time, and returns the time for "now"
--   as a convenience.
waitFor :: NominalDiffTime -> IO NominalDiffTime
waitFor targetTime = do currentTime <- getPOSIXTime
                        case compare currentTime targetTime of
                            LT -> do threadDelay $ floor ((targetTime - currentTime) * 1e6)
                                     getPOSIXTime
                            EQ -> return currentTime
                            GT -> do debugM "Game.Engine.update"
                                       $ "Missed the framerate deadline by " ++ show ((ceiling $ (currentTime - targetTime)*1e6) :: Integer) ++ " μs."
                                     getPOSIXTime

-- | How long a frame is, in seconds.
framePeriod :: NominalDiffTime
framePeriod = realToFrac $ 1 % targetFramerate

-- | The GLUT 'displayCallback' hook.
display :: GameState a -> Window -> IO ()
display gs w = do (d, rend, ls) <- modifyMVar (loaders gs) $ \ls ->
                       do ls' <- runGraphics $ runLoadersDeferred ls
                          (d, r) <- atomically $ do rend <- readTVar $ toRender gs
                                                    d   <- readTVar $ windowDims gs
                                                    return (d, rend)
                          return (ls', (d, r, ls'))
                  runGraphics $ updateWindow ls d rend
                              >> postRedisplay (Just w)

-- | The GLUT 'reshapeCallback' hook.
reshape :: GameState a -> Size -> IO ()
reshape gs sz@(Size x y) = do atomically $ writeTVar (windowDims gs) (fromIntegral x, fromIntegral y)
                              runGraphics $ do viewport $= (Position 0 0, sz)
                                               matrixMode $= Projection
                                               loadIdentity
                                               ortho2D 0 (fromIntegral x) 0 (fromIntegral y)
                              infoM "Game.Engine.reshape" $ "Window dimensions: " ++ show x ++ "x" ++ show y

-- | Initializes a new window, and returns its dimensions.
initWindow :: T.Text -> Dimensions -> GL Window
initWindow title dims  = do w <- createWindow title
                            currentWindow $= Just w

                            windowSize $= uncurry (Size `on` fromIntegral) dims

                            -- Enable antialiasing, and general graphical nicities.
                            lineSmooth $= Enabled
                            pointSmooth $= Enabled
                            polygonSmooth $= Enabled
                            blend $= Enabled
                            blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
                            lineWidth $= 2

                            mapM_ (\ty -> hint ty $= Nicest) [ PointSmooth
                                                            , LineSmooth
                                                            , PolygonSmooth
                                                            ]

                            return w
