module Main (main) where

import Chess()
import Config
import Data.List
import Data.Maybe
import qualified Graphics.Rendering.OpenGL.Monad as GL
import System.IO (stderr)
import System.Log.Formatter
import System.Log.Handler as H
import System.Log.Handler.Simple
import System.Log.Logger as L
import UI.Colors
import UI.Render
import UI.TextureCache

import Graphics.UI.GLUT

-- | Initializes all the loggers' states to what was defined in the config file.
configLogger :: IO ()
configLogger = do root <- getRootLogger

                  let formatter' = simpleLogFormatter logFormat

                  consoleOutput <- streamHandler stderr logLevel

                  let log' = foldl' (flip ($)) root
                        [ L.setLevel logLevel
                        , setHandlers $ map (`H.setFormatter` formatter')
                              [ consoleOutput ]
                        ]

                  -- Apply the changes to the global logger.
                  saveGlobalLogger log'

                  -- Set up all our custom logger levels.
                  mapM_ (\(logName, prio) -> updateGlobalLogger logName $ L.setLevel prio) customLogLevels

data GameState = GameState { textures :: [(String, Texture)]
                           , cache :: TextureCache
                           }

display :: GameState -> DisplayCallback
display gs = do let rectangle = (rectangleRenderer 10 10 red)
                                    { vAlign = Just (TopAlign $ -10)
                                    , hAlign = Just (LeftAlign 10)
                                    , rotation = pi/4
                                    , rotateAround = (5, 5)
                                    }

                    yellowDot = textureRenderer . fromJust . lookup "yellow-dot.png" $ textures gs

                (Size x y) <- get windowSize

                GL.runGraphics . updateWindow (fromIntegral x, fromIntegral y)
                    $ yellowDot { vAlign = Just (VCenterAlign 0)
                                , hAlign = Just (HCenterAlign 0)
                                , children = [rectangle]
                                }

myInit :: Window -> TextureCache -> IO ()
myInit w tc = do currentWindow $= Just w

                 -- select clearing color
                 clearColor $= clampify (white `withAlpha` 1)

                 Size windowWidth windowHeight <- get windowSize

                 infoM "Main.myInit" $ "Window dimensions: " ++ show windowWidth ++ "x" ++ show windowHeight

                 -- Enable antialiasing, and general graphical nicities.
                 lineSmooth $= Enabled
                 pointSmooth $= Enabled
                 polygonSmooth $= Enabled
                 blend $= Enabled
                 blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
                 lineWidth $= 1.5

                 mapM_ (\ty -> hint ty $= Nicest) [ PointSmooth
                                                 , LineSmooth
                                                 , PolygonSmooth
                                                 ]

                 -- initialize viewing values
                 matrixMode $= Projection
                 loadIdentity
                 ortho2D 0 (fromIntegral windowWidth) 0 (fromIntegral windowHeight)

                 -- preload our texture cache.
                 preloadTextures tc [ "yellow-dot.png" ]

-- Declare initial window size, position, and display mode (single buffer and
-- RGBA). Open window with "hello" in its title bar. Call initialization
-- routines. Register callback function to display graphics. Enter main loop and
-- process events.
main :: IO ()
main = do configLogger
          _ <- getArgsAndInitialize
          initialDisplayMode $= [ DoubleBuffered, RGBMode ]
          initialWindowSize $= Size 800 600
          initialWindowPosition $= Position 0 0
          w <- createWindow "hello, world!"
          tc <- newTextureCache
          myInit w tc
          tex <- loadTexture tc "yellow-dot.png"
          displayCallback $= display GameState { textures = [("yellow-dot.png", fromJust tex)]
                                               , cache = tc
                                               }
          mainLoop
