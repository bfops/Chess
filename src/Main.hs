module Main (main) where

import Chess()
import Config
import System.Log.Logger
import UI.Render

import Graphics.UI.GLUT

display :: Window -> DisplayCallback
display w = do let rectangle = (rectangleRenderer 100 100 (Color3 1.0 1.0 (1.0 :: GLfloat)))
                                 { vAlign = Just (VCenterAlign 0)
                                 , hAlign = Just (HCenterAlign 0)
                                 }

               updateWindow w rectangle

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
main = do updateGlobalLogger rootLoggerName (setLevel logLevel)
          _ <- getArgsAndInitialize
          initialDisplayMode $= [ DoubleBuffered, RGBMode ]
          initialWindowSize $= Size 800 600
          initialWindowPosition $= Position 0 0
          w <- createWindow "hello, world!"
          myInit w
          displayCallback $= display w
          mainLoop
