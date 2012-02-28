{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main (main) where

import Chess()
import Config
import Data.List
import Debug.Trace
import Game.Input
import Game.Engine
import Game.ResourceLoader
import Graphics.Rendering.OpenGL.Monad as GL
import System.IO (stderr)
import System.Log.Formatter
import System.Log.Handler as H
import System.Log.Handler.Simple
import System.Log.Logger as L
import UI.Colors
import UI.Render
import Util.Defs
import Util.HashString

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

                  let addLogLevel (l, prio) = updateGlobalLogger l $
                                                  L.setLevel prio

                  -- Set up all our custom logger levels.
                  mapM_ addLogLevel Config.customLogLevels

data GameState = GameState { shouldShow :: Bool -- Should the scene be rendered?
                           , rectPos :: Coord
                           }

display :: GameState -> Dimensions -> Loaders -> GL ()
display gs dims ls = let tex = getResource (textureL ls) "yellow-dot.png"
                         rect = (rectangleRenderer 200 200 red)
                                  { pos = Left $ rectPos gs
                                  , children = [ dot ]
                                  }
                         dot = (textureRenderer tex)
                                  { pos = Right ( HCenterAlign 0
                                                , VCenterAlign 0
                                                )
                                  , rotation = -pi/4
                                  }
                     in if shouldShow gs then updateWindow dims rect
                                         else return ()

-- | Solves for the new position of the rectangle, using w-a-s-d as movement.
solveNewPos :: Coord -> InputState -> Coord
solveNewPos _ is = mousePos is

-- | We don't do anything... for now.
update :: GameState -> Double -> InputState -> IO (GameState, [ResourceRequest])
update gs _ is = return ( GameState { shouldShow = True,
                                      rectPos = solveNewPos (rectPos gs) is
                                    }
                        , [ Loaded [hashed|yellow-dot.png|]
                          ] )

initState :: GameState
initState = GameState False (100, 100)

-- Call initialization routines. Register callback function to display
-- graphics. Enter main loop and process events.
main :: IO ()
main = do configLogger
          runGame Config.windowTitle initState display update
