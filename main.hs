module Main (main) where

import Graphics.UI.GLUT

display :: DisplayCallback
display = do clear [ ColorBuffer ]

             -- draw white polygon (rectangle) with corners at
             -- (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0)
             color (Color3 1.0 1.0 (1.0 :: GLfloat))
             -- resolve overloading, not needed in "real" programs
             let vertex3f = vertex :: Vertex3 GLfloat -> IO ()
             renderPrimitive Polygon $ mapM_ vertex3f [
                 Vertex3 0.25 0.25 0.0,
                 Vertex3 0.75 0.25 0.0,
                 Vertex3 0.75 0.75 0.0,
                 Vertex3 0.25 0.75 0.0]

             -- start processing buffered OpenGL routines
             swapBuffers

myInit :: IO ()
myInit = do
   -- select clearing color
   clearColor $= Color4 0 0 0 0

   -- initialize viewing values
   matrixMode $= Projection
   loadIdentity
   ortho 0 1 0 1 (-1) 1

-- Declare initial window size, position, and display mode (single buffer and
-- RGBA). Open window with "hello" in its title bar. Call initialization
-- routines. Register callback function to display graphics. Enter main loop and
-- process events.
main :: IO ()
main = do
   _ <- getArgsAndInitialize
   initialDisplayMode $= [ DoubleBuffered, RGBMode ]
   initialWindowSize $= Size 800 600
   initialWindowPosition $= Position 0 0
   _ <- createWindow "hello, world!"
   myInit
   displayCallback $= display
   mainLoop
