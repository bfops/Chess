module Main (main) where

import Chess()
import Config
import Data.List
import Game.Engine
import Graphics.Rendering.OpenGL.Monad as GL
import System.IO (stderr)
import System.Log.Formatter
import System.Log.Handler as H
import System.Log.Handler.Simple
import System.Log.Logger as L
import UI.Colors
import UI.Render
import UI.TextureLoader()

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

data GameState = GameState { renderers :: Renderer
                           }

display :: GameState -> Dimensions -> GL ()
display gs dims = updateWindow dims $ renderers gs

-- | We don't do anything... for now.
update :: GameState -> Double -> IO GameState
update gs _ = return gs

-- | Event handling is currently unimplemented.
onEvent :: GameState -> Event -> GameState
onEvent gs _ = gs

-- | TODO: Textures!?
initState :: GameState
initState = GameState $ (rectangleRenderer 200 200 red)
                            { pos = (200, 200)
--                            , rotation = pi/4
--                            , rotateAround = (5, 5)
                            }

-- Declare initial window size, position, and display mode (single buffer and
-- RGBA). Open window with "hello" in its title bar. Call initialization
-- routines. Register callback function to display graphics. Enter main loop and
-- process events.
main :: IO ()
main = do configLogger
          runGame "Chess - By B & C" initState display update onEvent
