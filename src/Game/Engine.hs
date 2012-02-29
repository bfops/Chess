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
import qualified Data.Text     as T
import           Data.Ratio
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import Game.Input as I
import Game.ResourceLoader
import Game.Texture

import           Graphics.Rendering.OpenGL.Monad
import           Graphics.UI.GLUT ( Position(..)
                                  , mainLoop
                                  )

import System.Log.Logger

import Util.Defs

data GameState s = GameState { userState  :: TVar s
                             , windowDims :: TVar Dimensions
                             , inputSt    :: TVar InputState
                             , loaders    :: MVar Loaders
                             }

-- | The resource loaders associated with any running game.
data Loaders = Loaders { textureL :: ResourceLoader DynamicImage Texture
                       }

-- | Starts the main loop of a game. Games will be run full screen and at the
--   maximum possible resolution.
runGame :: T.Text
        -- ^ The title of the game window.
        -> gameState
        -- ^ The initial game's state.
        -> (gameState -> Dimensions -> Loaders -> GL ())
        -- ^ The 'render' function. Turns the state into something draw-able.
        --   This should do the minimum amount of work possible. All the hard
        --   stuff should be handled in the update function. The 'Dimensions'
        --   parameter is the current resolution of the display. Respect it!
        -> (gameState -> Double -> InputState -> IO (gameState, [ResourceRequest]))
        -- ^ Updates the game's state, given a current time (in seconds), and
        --   the keyboard/mouse input vector. All computationally expensive
        --   tasks should be done in here.
        --
        --   Resources required for rendering the described scene must be
        --   specified in the second element of the returned tuple.
        -> IO ()
runGame title initState rend updateT = do runGraphics $ getArgsAndInitialize
                                                      >> initialDisplayMode $= [ DoubleBuffered
                                                                              , RGBAMode
                                                                              , Multisampling
                                                                              , WithSamplesPerPixel 2
                                                                              ]

                                          w <- runGraphics $ initWindow title windowDimensions

                                          state <- GameState <$> atomically (newTVar initState)
                                                            <*> atomically (newTVar windowDimensions)
                                                            <*> atomically (newTVar I.empty)
                                                            <*> newMVar    (Loaders emptyResourceLoader)

                                          runGraphics $ do displayCallback       $= display state w rend
                                                           reshapeCallback       $= Just (reshape state)
                                                           keyboardMouseCallback $= Just (onKeyMouse (inputSt state) $ windowDims state)
                                                           motionCallback        $= Just (onMotion   (inputSt state) $ windowDims state)
                                                           passiveMotionCallback $= Just (onMotion   (inputSt state) $ windowDims state)

                                          tid <- forkIO $ runUpdateLoop state updateT

                                          mainLoop
                                          killThread tid

runUpdateLoop :: GameState a
              -> (a -> Double -> InputState -> IO (a, [ResourceRequest]))
              -> IO ()
runUpdateLoop gs updateT = getPOSIXTime >>= go 0
    where
        --   frame #        t1
        go :: Integer -> NominalDiffTime -> IO ()
        go !n t1 = do t2 <- waitFor (t1 + framePeriod)
 
                      us <- atomically . readTVar $ userState gs
                      is <- atomically . readTVar $ inputSt gs

                      -- Update! We let the render thread read the old state while
                      -- we're updating, and clobber it afterwards. Although we
                      -- don't run it in the STM monad, the state is guaranteed
                      -- to be read-only while we update. If this is broken, fix
                      -- the offending code!
                      (us', reqs) <- updateT us (realToFrac t2) is

                      modifyMVar_ (loaders gs) $ \ls ->
                          do ls' <- updateLoaders ls reqs
                             atomically $ writeTVar (userState gs) us'
                             return ls'

                      go (n+1) t2

-- | Runs 'chooseResources' on all available loaders.
updateLoaders :: Loaders -> [ResourceRequest] -> IO Loaders
updateLoaders (Loaders tl) rs = Loaders <$> chooseResources tl rs

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
display :: GameState a -> Window-> (a -> Dimensions -> Loaders -> GL ()) -> IO ()
display gs w rend = do (u, d, ls) <- modifyMVar (loaders gs) $ \ls ->
                                     do ls' <- Loaders <$> (runGraphics . runDeferred $ textureL ls)
                                        (u, d) <- atomically $ do u <- readTVar $ userState gs
                                                                  d <- readTVar $ windowDims gs
                                                                  return (u, d)
                                        return (ls', (u, d, ls'))
                       runGraphics $ rend u d ls
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
