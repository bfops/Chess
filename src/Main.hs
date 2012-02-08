module Main (main) where

import Chess()
import Config
import Data.List
import System.IO (stderr)
import System.Log.Formatter
import System.Log.Handler as H
import System.Log.Handler.Simple
import System.Log.Logger as L
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
                        , setHandlers $ map (flip H.setFormatter formatter')
                              [ consoleOutput ]
                        ]

                  -- Apply the changes to the global logger.
                  saveGlobalLogger log'

                  -- Set up all our custom logger levels.
                  mapM_ (\(logName, prio) -> updateGlobalLogger logName $ L.setLevel prio) customLogLevels

display :: Window -> TextureCache -> DisplayCallback
display w tc = do let rectangle = (rectangleRenderer 10 10 (Color3 1.0 1.0 (1.0 :: GLfloat)))
                                 { vAlign = Just (TopAlign 10)
                                 , hAlign = Just (LeftAlign 10)
                                 }

                  yellowDot <- textureRenderer tc "yellow-dot.png"

                  updateWindow w $ yellowDot { vAlign = Just (VCenterAlign 0)
                                             , hAlign = Just (HCenterAlign 0)
                                             , children = [rectangle]
                                             }

myInit :: Window -> IO ()
myInit w = do currentWindow $= Just w
              -- select clearing color
              clearColor $= Color4 0 0 0 0

              Size windowWidth windowHeight <- get windowSize

              infoM "Main.myInit" $ "Window dimensions: " ++ show windowWidth ++ "x" ++ show windowHeight

              -- initialize viewing values
              matrixMode $= Projection
              loadIdentity
              ortho2D 0 (fromIntegral windowWidth) 0 (fromIntegral windowHeight)

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
          myInit w
          displayCallback $= display w tc
          mainLoop
