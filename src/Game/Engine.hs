-- | In this module, we provide a basic framework for running a game.
module Game.Engine ( runGame
                   , Event(..)
                   , Loaders ( textureL )
                   , Dimensions
                   ) where

import           Config

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.DeepSeq

import qualified Data.Foldable as F
import           Data.Function
import qualified Data.Text     as T
import           Data.Ratio
import           Data.Sequence ( Seq, (|>) )
import qualified Data.Sequence as Seq
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import Game.ResourceLoader
import Game.Texture

import           Graphics.Rendering.OpenGL.Monad
import           Graphics.UI.GLUT ( Key(..)
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

data GameState s = GameState { userState  :: TVar s
                             , windowDims :: TVar Dimensions
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
        -> (gameState -> Double -> IO (gameState, [ResourceRequest]))
        -- ^ Updates the game's state, given a current time (in seconds).
        --   All computationally expensive tasks should be done in here.
        --
        --   All resources required for rendering the described scene must be
        --   specified in the second element of the returned tuple.
        -> (gameState -> Event -> gameState)
        -- ^ Updates the game's state given some input by the user.
        -> IO ()
runGame title initState rend updateT updateE = do runGraphics $ getArgsAndInitialize
                                                              >> initialDisplayMode $= [ DoubleBuffered
                                                                                      , RGBAMode
                                                                                      , WithSamplesPerPixel 2
                                                                                      ]

                                                  w <- runGraphics $ initWindow title windowDimensions

                                                  uState <- atomically $ newTVar initState
                                                  winDims <- atomically $ newTVar windowDimensions
                                                  loads <- newMVar $ Loaders emptyResourceLoader

                                                  eventQ <- atomically $ newTVar Seq.empty

                                                  let state = GameState { userState = uState
                                                                        , windowDims = winDims
                                                                        , loaders = loads
                                                                        }
                                                   in do runGraphics $ do displayCallback       $= display state rend
                                                                          reshapeCallback       $= Just (reshape state)
                                                                          keyboardMouseCallback $= Just (onKeyMouse eventQ)
                                                                          motionCallback        $= Just (onMotion eventQ)

                                                         tid <- forkIO $ runUpdateLoop state updateT updateE eventQ

                                                         mainLoop
                                                         killThread tid

runUpdateLoop :: GameState a
              -> (a -> Double -> IO (a, [ResourceRequest]))
              -> (a -> Event -> a)
              -> TVar (Seq Event)
              -> IO ()
runUpdateLoop gs updateT updateE eventQ = getPOSIXTime >>= go
    where
        go :: NominalDiffTime -> IO ()
        go t1 = do t2 <- waitFor (t1 + framePeriod)
                   
                   us <- atomically . readTVar $ userState gs

                   -- Update! We let the render thread read the old state while
                   -- we're updating, and clobber it afterwards. Although we
                   -- don't run it in the STM monad, the state is guaranteed
                   -- to be read-only while we update. If this is broken, fix
                   -- the offending code!
                   us'  <- applyEventQueue us eventQ updateE
                   (us'', reqs) <- updateT us' $ realToFrac t2

                   modifyMVar_ (loaders gs) $ \ls ->
                        do ls' <- updateLoaders ls reqs
                           atomically $ writeTVar (userState gs) us''
                           return ls'

                   go t2

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

    return $ F.foldl updateE gs q

-- | The GLUT 'displayCallback' hook.
display :: GameState a -> (a -> Dimensions -> Loaders -> GL ()) -> IO ()
display gs rend = do (u, d, ls) <- modifyMVar (loaders gs) $ \ls ->
                                     do ls' <- Loaders <$> (runGraphics . runDeferred $ textureL ls)
                                        (u, d) <- atomically $ do u <- readTVar $ userState gs
                                                                  d <- readTVar $ windowDims gs
                                                                  return (u, d)
                                        return (ls', (u, d, ls'))
                     runGraphics $ rend u d ls

-- | The GLUT 'reshapeCallback' hook.
reshape :: GameState a -> Size -> IO ()
reshape gs sz@(Size x y) = do atomically $ writeTVar (windowDims gs) (fromIntegral x, fromIntegral y)
                              runGraphics $ do viewport $= (Position 0 0, sz)
                                               matrixMode $= Projection
                                               loadIdentity
                                               ortho2D 0 (fromIntegral x) 0 (fromIntegral y)
                              infoM "Game.Engine.reshape" $ "Window dimensions: " ++ show x ++ "x" ++ show y

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
