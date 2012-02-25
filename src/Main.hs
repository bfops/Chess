module Main (main) where

import           Chess()
import qualified Config
import           Data.Function
import           Data.List
import           Game.Engine
import           Graphics.Rendering.OpenGL.Monad as GL
import           Graphics.UI.GLUT ( Window
                                  , createWindow
                                  )
import           System.IO (stderr)
import           System.Log.Formatter
import           System.Log.Handler as H
import           System.Log.Handler.Simple
import           System.Log.Logger as L
import           UI.Colors
import           UI.Render
import           UI.TextureLoader()

-- | Initializes all the loggers' states to what was defined in the config file.
configLogger :: IO ()
configLogger = do root <- getRootLogger

                  let formatter' = simpleLogFormatter Config.logFormat

                  consoleOutput <- streamHandler stderr Config.logLevel

                  let log' = foldl' (flip ($)) root
                        [ L.setLevel Config.logLevel
                        , setHandlers $ map (`H.setFormatter` formatter')
                              [ consoleOutput ]
                        ]

                  -- Apply the changes to the global logger.
                  saveGlobalLogger log'

                  let addLogLevel (log, prio) = updateGlobalLogger log $
                                                    L.setLevel prio

                  -- Set up all our custom logger levels.
                  mapM_ addLogLevel Config.customLogLevels

data GameState = GameState { renderers :: Renderer
                           }

display :: GameState -> Dimensions -> GL ()
display gs dims = updateWindow dims $ renderers gs

-- | We don't do anything... for now.
update :: GameState -> Double -> GameState
update gs _ = gs

-- | Event handling is currently unimplemented.
onEvent :: GameState -> Event -> GameState
onEvent gs _ = gs

-- | TODO: Textures!?
initState :: GameState
initState = GameState $ (rectangleRenderer 200 200 red)
                            { pos = Right (HCenterAlign 0, VCenterAlign 0)
                            , rotation = pi/4096
                                      -- ^ Magic hack because 0 doesn't work on my machine.
                            }

-- | Initializes a new window, and returns its dimensions.
initWindow :: Window -> GL Dimensions
initWindow w = do currentWindow $= Just w

                  windowSize $= uncurry (Size `on` fromIntegral) Config.windowDimensions

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

                  return Config.windowDimensions


-- Call initialization routines. Register callback function to display
-- graphics. Enter main loop and process events.
main :: IO ()
main = do configLogger
          runGraphics $ getArgsAndInitialize
                      >> initialDisplayMode $= [ DoubleBuffered
                                               , RGBAMode
                                               , WithSamplesPerPixel 2
                                               ]

          dims <- runGraphics . initWindow =<< createWindow Config.windowTitle
          runGame dims initState display update onEvent
